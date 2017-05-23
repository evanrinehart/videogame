{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FRP
( E
, R
, Time
, runFRP
, fromListE
, neverE
, mergeE
, justE
, spawnerE
, terminateE
, timeShiftE
, mapWithTimeE

, accum
, snapshot
, snapshot_
, switcher
, transitions
-- spawnerR
, timeShiftR

-- enter/exitR/E
)
where

import Data.Fixed
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Applicative
import Data.Function
import System.IO
import System.IO.Unsafe
import System.Clock
import Data.IORef
import Control.Monad
import System.Mem.Weak
import System.Random
import Data.List
import Data.Monoid
import GHC.Prim
import qualified GHC.Conc as C

import Data.Char

type Time = Nano

data TimeError = TimeError Int deriving Show
instance Exception TimeError

-- TimeVar

-- A pure value which may not be known until some time. You can compare
-- the timestamps of two timevars and get the answer as soon as possible.

data TimeVar a = TimeVar 
  { readTimeVar    :: (Time,a)
  , tryReadTimeVar :: STM (Maybe (Time,a)) }
      deriving Functor

data Sooner a b =
  AWins Time a |
  BWins Time b |
  ABTie Time a b
    deriving Show

newTimeVarIO :: IO (TimeVar a, Time -> a -> IO ())
newTimeVarIO = do
  mv <- newEmptyTMVarIO
  let write t x = atomically (tryPutTMVar mv (t,x) >> return ())
{-
  let value = unsafePerformIO $ catch
                (atomically (readTMVar mv))
                (\(e :: BlockedIndefinitelyOnSTM) -> hang)
-}
  let value = unsafePerformIO (atomically (readTMVar mv))
  let tryRead = tryReadTMVar mv
  return (TimeVar value tryRead, write)

pureTimeVar :: Time -> a -> TimeVar a
pureTimeVar t x = TimeVar
  (t,x)
  (return (Just (t,x)))

readTimeVarIO :: TimeVar a -> IO (Time,a)
readTimeVarIO i = do
  let v = readTimeVar i
  evaluate v
  return v

data Rumsfeld a b =
  KnownKnown (Time,a) (Time,b) |
  KnownUnknown (Time,a) |
  UnknownKnown (Time,b) |
  UnknownUnknown

rumsfeldTest :: TimeVar a -> TimeVar b -> STM (Rumsfeld a b)
rumsfeldTest tv1 tv2 = do
  m1 <- tryReadTimeVar tv1
  m2 <- tryReadTimeVar tv2
  case (m1,m2) of
    (Nothing, Nothing) -> return UnknownUnknown
    (Just r@(t,x), Nothing) -> return (KnownUnknown r)
    (Nothing, Just r@(t,x)) -> return (UnknownKnown r)
    (Just r1@(t1,x1), Just r2@(t2,x2)) -> return (KnownKnown r1 r2)

soonerTimeVar :: TimeVar a -> TimeVar b -> Sooner a b
soonerTimeVar tv1 tv2 = unsafePerformIO $ fix $ \restart -> do
  r <- atomically (rumsfeldTest tv1 tv2)
  now <- getCurrentGlobalTime
  let easy = case r of
        KnownKnown (t1,x1) (t2,x2)
          | t1 < t2   -> Right (AWins t1 x1)
          | t2 < t1   -> Right (BWins t2 x2)
          | otherwise -> Right (ABTie t1 x1 x2)
        KnownUnknown (t,_) -> Left t
        UnknownKnown (t,_) -> Left t
        UnknownUnknown -> Left (now + 5)
  case easy of
    Right ans -> return ans
    Left target -> do
      clock <- newTVarIO now 
      result <- withAsync (do
          threadDelay (max 0 (floor ((target - now) * 1000000)))
          now <- getCurrentGlobalTime
          atomically (writeTVar clock now) ) $ \waker -> do
        atomically $ do
          t <- readTVar clock
          r <- rumsfeldTest tv1 tv2
          case r of
            KnownKnown e1@(t1,x1) e2@(t2,x2)
              | t1 < t2   -> return (Just (AWins t1 x1))
              | t2 < t1   -> return (Just (BWins t2 x2))
              | otherwise -> return (Just (ABTie t1 x1 x2))
            KnownUnknown (t1, x)
              | t < t1    -> retry
              | otherwise -> return (Just (AWins t1 x))
            UnknownKnown (t2, x)
              | t < t2    -> retry
              | otherwise -> return (Just (BWins t2 x))
            UnknownUnknown
              | t < target -> retry
              | otherwise  -> return Nothing
      case result of
        Nothing -> restart
        Just ans -> return ans

data Sooner' a =
  TimeFirst |
  TimeVarFirst Time a |
  SameTime a
    deriving (Show,Functor)

soonerTime :: Time -> TimeVar a -> Sooner' a
soonerTime t i = case soonerTimeVar (pureTimeVar t undefined) i of
  AWins _ _ -> TimeFirst
  BWins t x -> TimeVarFirst t x
  ABTie t _ y -> SameTime y

-- cell status

-- the nature of a location in a reactive stream

data CellStatus a =
  Dormant Time a |
  Transit Time a a |
  Initial a
    deriving (Functor, Show)

cellValuePlus :: CellStatus a -> a
cellValuePlus (Dormant _ x) = x
cellValuePlus (Transit _ _ x) = x
cellValuePlus (Initial x) = x

cellValueMinus :: CellStatus a -> a
cellValueMinus (Dormant _ x) = x
cellValueMinus (Transit _ x _) = x
cellValueMinus (Initial x) = x

lessThanLastCellTime :: Time -> CellStatus a -> Bool
lessThanLastCellTime t (Initial _) = False
lessThanLastCellTime t (Dormant t' _) = t < t'
lessThanLastCellTime t (Transit t' _ _) = t < t'

greaterThanLastCellTime :: Time -> CellStatus a -> Bool
greaterThanLastCellTime t (Initial _) = True
greaterThanLastCellTime t (Dormant t' _) = t > t'
greaterThanLastCellTime t (Transit t' _ _) = t > t'

instance Applicative CellStatus where
  pure x = Initial x
  (Initial f) <*> (Initial x) = Initial (f x)
  (Initial f) <*> (Dormant t2 x) = Dormant t2 (f x)
  (Initial f) <*> (Transit t2 x1 x2) = Transit t2 (f x1) (f x2)
  (Dormant t1 f) <*> (Dormant t2 x) = Dormant (max t1 t2) (f x)
  (Dormant t1 f) <*> (Transit t2 x x') = Transit (max t1 t2) (f x) (f x')
  (Dormant t1 f) <*> (Initial x) = Dormant t1 (f x)
  (Transit t1 f1 f2) <*> (Dormant t2 x) = Transit (max t1 t2) (f1 x) (f2 x)
  (Transit t1 f1 f2) <*> (Transit t2 x1 x2) = Transit (max t1 t2) (f1 x1) (f2 x2)
  (Transit t1 f1 f2) <*> (Initial x) = Transit t1 (f1 x) (f2 x)


-- R

-- R is a piecewise constant function of time. Implemented as a list of
-- cell statuses.

data R a = RPure (CellStatus a) | RCons (CellStatus a) (TimeVar (R a))

cellR :: R a -> CellStatus a
cellR (RPure v) = v
cellR (RCons v _) = v

instance Functor R where
  fmap f (RPure v) = RPure (fmap f v)
  fmap f (RCons v i) = RCons (fmap f v) (fmap (fmap f) i)

instance Applicative R where
  pure x = RPure (Initial x)
  RCons f i1 <*> RCons x i2 = error "R appl"
  RCons f i1 <*> RPure x = error "R appl"
  RPure f <*> RCons x i2 = error "R appl"
  RPure f <*> RPure x = RPure (f <*> x)

instance Monad R where
  return = pure
  r >>= f = joinR (fmap f r)

instance Show a => Show (R a) where
  showsPrec d (RPure v) = showParen (d > 10) $
    showString  "RPure " . showsPrec 11 v
  showsPrec d (RCons v i) = let (t,r') = readTimeVar i in
    showParen (d > 5)
    ( showString "RCons " .
      showsPrec 6 v .
      showString " " .
      showsPrec 6 r' )

headR :: R a -> a
headR (RPure v) = cellValuePlus v
headR (RCons v _) = cellValueMinus v

nextR :: R a -> Maybe (Time, R a)
nextR (RPure _) = Nothing
nextR (RCons _ i) = Just (readTimeVar i)

seekR :: Time -> R a -> R a
seekR t r = case r of
  RPure cell -> let !x = cellValuePlus cell in RPure (Dormant t x)
  RCons cell i
    | Just t <= cellStart cell -> r
    | otherwise -> case soonerTime t i of
        TimeFirst -> RCons (Dormant t (cellValuePlus cell)) i
        TimeVarFirst _ r' -> seekR t r'
        SameTime r' -> r'

accum :: a -> E (a -> a) -> R a
accum x0 e0 = go (Initial x0) e0 where
  go v e = case nextE e of
    Nothing -> RPure v
    Just (t,f,e') -> RCons v (pureTimeVar t r') where
      x0 = cellValuePlus v
      x1 = f x0
      r' = go (Transit t x0 x1) e'

--maxCellStatus :: CellStatus a -> CellStatus a -> CellStatus a
--maxCellStatus 

adjustR :: (CellStatus a -> CellStatus a) -> R a -> R a
adjustR f (RPure cell) = RPure (f cell)
adjustR f (RCons cell i) = RCons (f cell) i

truncateCell :: Time -> a -> CellStatus a -> CellStatus a
truncateCell newT newPriorX cell = case cell of
  Initial x -> Transit newT newPriorX x
  Transit t x1 x2
    | newT >= t -> Transit newT newPriorX x2
    | otherwise -> cell
  Dormant t x
    | newT >= t -> Transit newT newPriorX x
    | otherwise -> cell

cellStart :: CellStatus a -> Maybe Time
cellStart (Initial _) = Nothing
cellStart (Transit t _ _) = Just t
cellStart (Dormant t _) = Just t


joinR :: forall a . R (R a) -> R a
joinR rr = go (headR rr) (transitions rr) where
  go :: R a -> E (R a, R a) -> R a
  go r switch = case nextE switch of
    Nothing -> r
    Just (tSwitch, (_,nextR), switch') ->
      let cell = cellR r in
      if cellStart cell == Just tSwitch
      then -- skip cell, adjust, switch
        let x = cellValueMinus cell in
        let r' = adjustR (truncateCell tSwitch x) (seekR tSwitch nextR) in
        go r' switch'
      else if cellStart cell < Just tSwitch
      then -- do one of two moves, accept this cell and move on
           -- or accept this cell, truncate next behavior and switch.
        let x = cellValuePlus cell in
        let r' = adjustR (truncateCell tSwitch x) (seekR tSwitch nextR) in
        let standardCut = RCons cell (pureTimeVar tSwitch (go r' switch')) in
        case r of
          RPure _ -> standardCut
          RCons _ i -> case soonerTime tSwitch i of
            TimeFirst -> standardCut
            TimeVarFirst t r'' -> RCons cell (pureTimeVar t (go r'' switch))
            SameTime _ -> standardCut
      else error "joinR bug 1"

switcher :: R a -> E (R a) -> R a
switcher start e = joinR $ accum start (fmap const e)

timeShiftCellStatus :: Time -> CellStatus a -> CellStatus a
timeShiftCellStatus dt (Dormant t x) = Dormant (t + dt) x
timeShiftCellStatus dt (Transit t x1 x2) = Transit (t + dt) x1 x2
timeShiftCellStatus dt v = v

mapStatusR :: (CellStatus a -> CellStatus b) -> R a -> R b
mapStatusR f r = go r where
  go (RPure v) = RPure (f v)
  go (RCons v i) = RCons (f v) (fmap go i)

timeShiftR :: Time -> R a -> R a
timeShiftR dt b = mapStatusR (timeShiftCellStatus dt) b

-- E

-- E is a sequence of timed values.

newtype E a = E [TimeVar a]

instance Monoid a => Monoid (E a) where
  mempty = neverE
  mappend = mergeE

instance Functor E where
  fmap f e = E (go e) where
    go e = case nextE e of
      Nothing -> []
      Just (t,x,e') -> pureTimeVar t (f x) : go e'


instance Show a => Show (E a) where
  show (E is) = show (map readTimeVar is)

nextE :: E a -> Maybe (Time, a, E a)
nextE (E []) = Nothing
nextE (E (i:is)) =
  let (t,x) = readTimeVar i in
  Just (t, x, E is)

seekE :: Time -> E a -> E a
seekE t (E []) = E []
seekE t e@(E (i:is)) =
  let (t', x) = readTimeVar i in
  if t > t' then seekE t (E is) else e

fromListE :: [(Time,a)] -> E a
fromListE xs = E (go xs) where
  go [] = []
  go ((t,x):rest) = pureTimeVar t x : go rest

mergeE :: Monoid a => E a -> E a -> E a
mergeE (E xs) (E ys) = E (go xs ys) where
  go e1@(i:is) e2@(j:js) = case soonerTimeVar i j of
    AWins t x   -> pureTimeVar t x : go is e2
    BWins t x   -> pureTimeVar t x : go e1 js
    ABTie t x y -> pureTimeVar t (mappend x y) : go is js
  go is [] = is
  go [] js = js

neverE :: E a
neverE = E []

justE :: E (Maybe a) -> E a
justE e = E (go e) where
  go (E []) = []
  go e = case nextE e of
    Nothing -> []
    Just (t,Nothing,e') -> go e'
    Just (t,Just x, e') -> pureTimeVar t x : go e'

snapshot :: E (a -> b) -> R a -> E b
snapshot e b = unsafePerformIO $ do
  core <- newIORef b
  anchor <- newMVar core
  ch <- dupChan globalHousekeepingChan
  w <- mkWeakMVar anchor (return ())
  forkIO (spookySnapshotThread w ch)
  return $ magicEBuilder e $ \t f -> withMVar anchor $ \ref -> do
    b <- readIORef ref
    when (t `lessThanLastCellTime` (cellR b)) (throwIO (TimeError 1))
    let b' = seekR t b
    let x = headR b'
    writeIORef ref b'
    return (f x)

magicEBuilder :: E a -> (Time -> a -> IO b) -> E b
magicEBuilder ein action = E (go ein) where
  go (E []) = []
  go (E (i:is)) =
    let (t,x) = readTimeVar i in
    pureTimeVar t (unsafePerformIO (action t x)) : go (E is)

spookySnapshotThread :: Weak (MVar (IORef (R a))) -> Chan Time -> IO ()
spookySnapshotThread w ch = loop where
  loop = do
    now <- readChan ch
    manchor <- deRefWeak w
    case manchor of
      Nothing -> do
        threadDelay 2000
        print "No magic container, I go poof"
      Just anchor -> do
        withMVar anchor $ \ref -> do
          b <- readIORef ref
          let v = cellR b
          when (now `greaterThanLastCellTime` v) $ do
            let !b' = seekR now b
            writeIORef ref b'
            threadDelay 1000
            print ("seeked to " ++ show now ++ " ... ")
        loop

snapshot_ :: E b -> R a -> E a
snapshot_ e b = snapshot (fmap (\_ -> id) e) b

transitions :: R a -> E (a,a)
transitions r = E (go r) where
  go r = case nextR r of
    Nothing -> []
    Just (t, r') ->
      let (Transit _ x0 x1) = cellR r' in
      pureTimeVar t (x0,x1) : go r'

terminateE :: E a -> E b -> E a
terminateE e ender = E (go e ender) where
  go e@(E (i:is)) ender@(E (j:js)) = case soonerTimeVar i j of
    AWins t x -> pureTimeVar t x : go (E is) ender
    BWins t _ -> []
    ABTie t x _ -> pureTimeVar t x : []
  go (E []) _ = []
  go (E is) (E []) = is

spawnerE :: Monoid a => E (E a) -> E a
spawnerE spawn = E (go spawn neverE) where
  go spawn@(E (sp:sps)) es@(E (i:is)) = case soonerTimeVar sp i of
    AWins t e -> go (E sps) (mergeE es (seekE t e))
    BWins t x -> pureTimeVar t x : go spawn (E is)
    ABTie t e x -> go (E sps) (mergeE (E is) (seekE t e))
  go spawn (E []) = case nextE spawn of
    Nothing -> []
    Just (t,e,spawn') -> go spawn' (seekE t e)
  go (E []) (E is) = is

timeShiftE :: Time -> E a -> E a
timeShiftE dt e = E (go e) where
  go e = case nextE e of
    Nothing -> []
    Just (t,x,e') -> pureTimeVar (t + dt) x : go e'

mapWithTimeE :: (Time -> a -> b) -> E a -> E b
mapWithTimeE f e = E (go e) where
  go e = case nextE e of
    Nothing -> []
    Just (t,x,e') -> pureTimeVar t (f t x) : go e'


-- runner

withOutputESink :: E a -> (Time -> a -> IO ()) -> (Async () -> IO b) -> IO b
withOutputESink e push action = withAsync (outputWorker e) $ action where
  outputWorker e = case nextE e of
    Just (!t,x,e') -> do
      now <- getCurrentGlobalTime
      when (now < t) $ do
        let micros = nanoToMicro (t - now)
        threadDelay micros
      push t x
      outputWorker e'
    Nothing -> return ()

withOutputRPoller :: R a -> (Time -> a -> IO ()) -> (Async () -> IO b) -> IO b
withOutputRPoller b push action = withAsync (outputWorker b) $ action where
  outputWorker b = forever $ do
    now <- getCurrentGlobalTime
    let !b' = seekR now b
    let !x = headR b'
    push now x

withExternalInputE :: IO a -> (E a -> Async () -> IO b) -> IO b
withExternalInputE pull action = do
  mv <- newEmptyMVar
  let magic = magicTimeVarSequence mv
  withAsync (inputWorker mv) $ action (E magic) where
  inputWorker mv = forever $ do
    x <- pull
    now <- getCurrentGlobalTime
    write <- takeMVar mv
    write now x

withPeriodicWinch :: (Async () -> IO b) -> IO b
withPeriodicWinch action = withAsync winch $ action where
  winch = forever $ do
    threadDelay 100000
    now <- getCurrentGlobalTime
    writeChan globalHousekeepingChan now

withHousekeepingVacuum :: (Async () -> IO b) -> IO b
withHousekeepingVacuum action = withAsync suck action where
  suck = do
    readChan globalHousekeepingChan
    suck

runFRP :: IO a -> (b -> IO ()) -> (c -> IO ()) -> (E a -> (R b, E c)) -> IO ()
runFRP getIn out1 out2 program = do
  setGlobalProgramStartTime
  withHousekeepingVacuum $ \vacA -> do
    withExternalInputE getIn $ \ein inA -> do
      let (rout, eout) = program ein
      withOutputESink eout (const out2) $ \out1A ->
        withOutputRPoller rout (const out1) $ \out2A ->
          withPeriodicWinch $ \winchA -> do
            (a, result) <- waitAnyCatch [inA, out1A, out2A, winchA, vacA]
            let who | a == inA = "input-worker"
                    | a == out1A = "output-worker"
                    | a == out2A = "output-poller"
                    | a == winchA = "winch"
                    | a == vacA = "vacuum"
                    | otherwise = "unknown"
            case result of
              Left err -> do
                hPutStrLn stderr (who ++ " crashed")
                throwIO err
              Right () -> do
                putStrLn (who ++ " ended")

nanoToMicro nano = floor (nano * 1000000)

-- magic

-- the way this works is, by looking at the sequence, you cause an IO action
-- to define the next element to be placed in the MVar. Then it outputs
-- a TimeVar which is yet unknown until something (another thread) takes that
-- action and uses it. At which time the stream can continue.
magicTimeVarSequence :: MVar (Time -> a -> IO ()) -> [TimeVar a]
magicTimeVarSequence mv = unsafePerformIO $ do
  (var, write) <- newTimeVarIO
  putMVar mv write
  return (var : magicTimeVarSequence mv)

hang :: IO a
hang = do
  threadDelay 10000000
  hang

-- globals

globalProgramStartTime :: TVar Time
globalProgramStartTime = unsafePerformIO (newTVarIO undefined)

setGlobalProgramStartTime :: IO ()
setGlobalProgramStartTime = do
  systime <- ((/ 1000000000) . fromInteger . toNanoSecs) <$> getTime Monotonic
  atomically (writeTVar globalProgramStartTime systime)

getCurrentGlobalTime :: IO Time
getCurrentGlobalTime = do
  epoch <- atomically (readTVar globalProgramStartTime)
  systime <- ((/ 1000000000) . fromInteger . toNanoSecs) <$> getTime Monotonic
  return (systime - epoch)

globalHousekeepingChan :: Chan Time
globalHousekeepingChan = unsafePerformIO newChan
