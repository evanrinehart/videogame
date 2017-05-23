{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FRP where

-- TimeVar a. This is a value at a time. The value of this variable becomes
-- known to us at some real time, and never changes, so is pure.
-- Code that tries to evaluate a TimeVar that is yet unknown will block
-- until it is.

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

type UnsafeTimeFunc a = Time -> (CellStatus a, [(Time,a)])




{-
timeZip :: (a -> b -> c) -> a -> [(Time,a)] -> b -> [(Time,b)] -> [(Time,c)]
timeZip f x0 arg1@((t1,x):xs) y0 arg2@((t2,y):ys)
  | t1 < t2 = (t1, f x y0) : timeZip f x xs y0 arg2
  | t2 < t1 = (t2, f x0 y) : timeZip f x0 arg1 y ys
  | otherwise = (t1, f x y) : timeZip f x xs y ys
timeZip _ _ _ _ _ = []
-}


-- TimeVar
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

snapshot :: Show a => E (a -> b) -> R a -> E b
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

spookySnapshotThread :: Show a => Weak (MVar (IORef (R a))) -> Chan Time -> IO ()
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

--snapshot_ :: E b -> R a -> E a
--snapshot_ e b = snapshot (fmap (\_ -> id) e) b

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

modernizeE :: E (E a) -> E (E a)
modernizeE es = mapWithTimeE timeShiftE es



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

prog :: E Int -> (R Bool, E String)
prog e = (image, out <> out2 <> fmap f e) where
  image = pure False
  out = fromListE [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e")]
  out2 = fromListE [(3,"a"),(4,"b"),(5,"c"),(6,"d"),(7,"e")]
  f i = [chr i]

fram0 :: IO Int
fram0 = threadDelay 1000000 >> return 13
--fram1 b = putStrLn ("poll: " ++ show b) >> threadDelay 105000
fram1 b = threadDelay 105000
fram2 c = putStrLn ("output: " ++ show c)

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


-- runner

-- the runner takes a (E Input -> (B Picture, E Output)) and needs to
-- be care to do the following the things start running the program.
-- also it takes a blocking, IO Input, a blocking, Picture -> IO ()
-- and a Output -> IO (). ok.
--
-- first first, set the global program start time.
-- first create an E Input from the IO Input using sourceEIO.
-- then create the renderer and output executors with sinkEIO.
-- ... the sourceEIO needs to get the global time when pushing inputs.
-- ... there needs to be a thread consuming and dropping the outputs of
-- the actual global housekeeping thread, since everything using that is using
-- a dup. finally we need a periodic write to the global housekeeping thread
-- with the time of the latest pull. when the video thread checks the world
-- at time t. right after that, do housekeeping for time t.

-- testing

loopTest :: Char -> R Char
loopTest c0 = n2 where
  n2 = accum c0 n1
  n1 = snapshot e n2
  e = fromListE [(1,const succ),(2,const succ),(3,const succ)]

-- test of time vars
runTest :: Nano -> Char -> Nano -> Char -> IO ()
runTest delay1 c1 delay2 c2 = do
  now <- getCurrentGlobalTime
  (t1,wr) <- newTimeVarIO
  let t2 = pureTimeVar (now + delay2) 'p'
  forkIO $ do
    threadDelay (floor (delay1 * 1000000))
    wr (now + delay1) '?'
  putStr "now = "
  print now
  print (soonerTimeVar t1 t2)


{-
hmm n = do
  let b = loopTest 'a'
  hmm2 b

hmm2 b = do
  threadDelay 1000000
  writeChan globalHousekeepingChan 3
  threadDelay 1000
  print (at b 3)
  threadDelay 1000000
  writeChan globalHousekeepingChan 4
  threadDelay 1000
  print (at b 4)
  threadDelay 1000000
  writeChan globalHousekeepingChan 5
  threadDelay 1000
  print (at b 5)
  threadDelay 1000000
  writeChan globalHousekeepingChan 6
  threadDelay 1000
  print (at b 6)
  threadDelay 1000000
  writeChan globalHousekeepingChan 7
  threadDelay 1000
  print (at b 7)
  threadDelay 1000000
  writeChan globalHousekeepingChan 8
  threadDelay 1000
  print (at b 8)
  hmm3 10

hmm3 0 = return ()
hmm3 n = do
  threadDelay 1000000
  writeChan globalHousekeepingChan (60 - n)
  print ("done " ++ show n)
  hmm3 (n-1)
-}
{-
test :: Int -> B Int
test n = accum n (fromListE [(1000,succ),(2000,succ),(3000,succ)])
-}


nanoToMicro nano = floor (nano * 1000000)

{-
testSink = do
  let e = fromListE [(0,'a'),(1,'b'),(2,'c')]
  setGlobalProgramStartTime
  sinkEIO e $ \t c -> do
    print (t,c)
-}

b1 :: R Char
b1 = pure 'a'

b2 :: R Char
b2 = accum 'x' es

es :: E (Char -> Char)
es = fromListE (zip [5,6..] (cycle [const 'y', const 'z']))

es2 :: E (R Char)
es2 = fromListE (zip [10,20..] (cycle [b1, b2]))

b3 :: R Char
b3 = switcher b1 es2

b4 :: R Integer
b4 = accum 0 (fromListE $ zip [1,2..] (repeat (succ)))

es3 = fromListE (zip [10,20..] (cycle [pure 0, b4]))

--b5 = switcher b4 es3

pulses = fromListE (zip [0,1..] (repeat ()))



{-
pulseGenerator :: R Double -> E ()
pulseGenerator freq = output where
  samplerPulses = zip [0.1, 0.2 .. ] (repeat ())
  samples = snapshot (fmap const samplerPulses) freq
  pulseBundles = fmap f samples
  f phi = fromListE [(0,()), (1, ()), (2, ())]
  output = spawnerE pulseBundles
-}

{-
how to use a pulse generator somewhere besides the beginning of time.
you need to shift the freq signal back by time equal to current time.
you need to shift the pulse outputs forward in time equal to current
time. the absolute time that you instantiate the pulse generator.

you have an R In -> E Out.
which expects R and E data to begin at t=0.

ok, so we can easily take an R happening in modern times, if we had it,
and shift it back to 0. after applying the above function to get an E,
we can shift it forward by the same amount to let it merge with the main 
output stream.

simple enough? well were does this R-in-modern times come from, which isnt
itself from the beginning of time and in dire need to seekR? realistically
this R may be part of a larger object the pulse generator is being used in.
ok. so that pushes the question down the line.

the horizontal motion machine for a hero will need a signal for "left" "right"
or "neutral". this signal is given as input to the hero as a whole. ok
where does that come from.  because we will probably want to instantiate the
hero as a whole, all this is happening at the same time.

ok, left/right/neutral is coming from a state machine which is part of the
hero, no actually we should probably leave that on the outside. fine. so
thats the interface. when the hero is dead, and respawns, right, so lets
say the hero is a single switcher. and to make it switch, for instance to
the "alive" state after a respawn, we need a E (R Hero). We also need a
E (E Sound) which happens at the same time so we can hear the footsteps.
these will react in the dedicated hero switcher. but where do you get the
proper E (R Hero). lets say you have a function
hero :: R HCtrl -> E Jump -> (R Hero, E Sound)
To get the R Hero and E Sound, we need R HCtrl and E Jump. The inputs should
come from the outside system already in progress, at non zero absolute time.
enterR :: R a -> E (R a -> b) -> E b
enterE :: E a -> E (E a -> b) -> E b
ok, so these are defined as, when the event happens, the version of R or E
argument at the time of the event, with past forgotten, is applied to the
contained function and the result is output. ... and also the R or E is then
shifted back to 0, based on the time of this event. one enterR followed by
an enterE would give us an E (R Hero, E Sound) from from an E carrying the
hero function. 

they are the right types, after fmap fst and fmap snd, to feed into the
switchers. but...
both outputs are relative to t = 0, as per the functions convention.
we cant really use them until they are fixed up, shifted forward by an 
amount equal to the instantiation time of the hero.

exitR :: R Time -> E (R a) -> E (R a)
exitE :: R Time -> E (E a) -> E (E a)

these two are defined as... whatever events or behaviors going through here
are shifted forward by the amount in the B Time, which would need to be set
by the same instantiation event as the hero. since we expect events and
behaviors entering to be shifted way back in the past, this would give data
situated right now in time. once you have *these* E (E a), E (R a)... you
can send them to switchers, whose output is merged into the main stream.
exitR and exitE can be implemented with regular primitives.

the other two... im not sure if they can be done with regular primitives.
here are new primitives that could do it.
R a -> E () -> E (R a), which is like snapshot but instead of sampling the
behavior, you get the behavior itself, frozen in time. this has major
potential to cause a space leak!

E a -> E () -> E (E a), is similar. it can be basically like a mergeE
but the left events are ignored and the right events cause the left event
tself to be output in raw form. if they happen at the same time, the event
the was about to happen is preserved in the output.  call them...

captureR captureE

so how do you encapsulate this so its not such a pain in the ass.

heroHarness ::
  R HCtrl -> E Jump -> E (R HCtrl -> E Jump -> (R Hero, E Sound)) -> 
  (R Hero, E Sound)
except the constructor is a constant. in reality you would use E () as
the spawn signal or E (A,B,C) for the start data, which would be the
first few arguments to the constructor. then the function payload would
not really be a constant anymore, but still, it would not be an input
to the harness. essentially, after this point, the player can play normally
until dead, in which case the switchers would get null data until a respawn
event happens. OK.

thats heros, what about items and enemies, both which form a dynamic collection.
so think about all the crap we did for (one) hero, how do we solve a whole
gaggle of goons!

this is getting big enough to go into a notes file.

-- tangent into doing IO actions somehow during this thing.

E (IO a) -> E (Either SomeException a)
when the event happens, the IO action begins asynchronously.
if another event happens before it comes back, they line up in a queue.
when the result is available, the output event occurs. could be useful.
how does it play into the FRP semantics. you could assign a sequence of
Time -> a -> (Time, Either SomeException a) to each such event. or you
would say the output events never occur... how useless!

-}
  

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
