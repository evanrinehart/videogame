{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
module Exp12 where

-- TimeVar a. This is a value at a time. The value of this variable becomes
-- known to us at some real time, and never changes, so is pure.
-- Code that tries to evaluate a TimeVar that is yet unknown will block
-- until it is.

import Data.Fixed
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Function
import System.IO.Unsafe
import System.Clock
import Data.IORef
import Control.Monad
import System.Mem.Weak
import System.Random
import GHC.Prim

import Data.Char

type Time = Nano
data TimeError = TimeError Int deriving Show

instance Exception TimeError

type UnsafeTimeFunc a = Time -> (CellStatus a, [(Time,a)])




timeZip :: (a -> b -> c) -> a -> [(Time,a)] -> b -> [(Time,b)] -> [(Time,c)]
timeZip f x0 arg1@((t1,x):xs) y0 arg2@((t2,y):ys)
  | t1 < t2 = (t1, f x y0) : timeZip f x xs y0 arg2
  | t2 < t1 = (t2, f x0 y) : timeZip f x0 arg1 y ys
  | otherwise = (t1, f x y) : timeZip f x xs y ys
timeZip _ _ _ _ _ = []


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

newTimeVar :: IO (TimeVar a, Time -> a -> IO ())
newTimeVar = do
  mv <- newEmptyTMVarIO
  let write t x = atomically (tryPutTMVar mv (t,x) >> return ())
  let value = unsafePerformIO $ atomically (readTMVar mv)
  let tryRead = tryReadTMVar mv
  return (TimeVar value tryRead, write)

pureTimeVar :: Time -> a -> TimeVar a
pureTimeVar t x = TimeVar
  (t,x)
  (return (Just (t,x)))

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

-- cell status

data CellStatus a =
  Dormant Time a |
  Transit Time a a |
  Initial a
    deriving (Functor, Show)

type CellGuts a = (IORef (CellStatus a), IORef (E (a -> a)))

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
  RCons f i1 <*> RCons x i2 = undefined
  RCons f i1 <*> RPure x = undefined
  RPure f <*> RCons x i2 = undefined
  RPure f <*> RPure x = RPure (f <*> x)

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
  RPure v -> let !x = cellValuePlus v in RPure (Dormant t x)
  RCons v i
    | t `greaterThanLastCellTime` v ->
        case soonerTimeVar (pureTimeVar t undefined) i of
          AWins _ _ -> let !x = cellValuePlus v in RCons (Dormant t x) i
          BWins t' r' -> seekR t r'
          ABTie _ _ r' ->
            let !x = cellValuePlus v in
            let !x' = headR r' in
            RCons (Transit t x x') i
    | otherwise -> r

at :: R a -> Time -> a
at r t =
  let !r' = seekR t r in
  headR r'

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

joinR :: R (R a) -> R a
joinR rr = go (headR rr) (transitions rr) where
  go r e = case nextE e of
    Nothing -> r
    Just (te, (_,origr'), e') ->
      let r' = seekR te origr' in
      case r of
        RPure x -> RCons x (pureTimeVar te (go r' e'))
        RCons x i -> case soonerTimeVar (pureTimeVar te undefined) i of
          AWins _ _ -> RCons x (pureTimeVar te (go r' e'))
          BWins t r'' -> RCons x (pureTimeVar t (go (seekR t r'') e))
          ABTie t _ _
            | t /= te -> error "impossible"
            | otherwise -> RCons (Transit t x0 x1) (pureTimeVar t (go r' e')) where
                x0 = cellValueMinus x
                x1 = headR r'

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
    AWins t e -> go (E sps) (mergeE es e)
    BWins t x -> pureTimeVar t x : go spawn (E is)
    ABTie t e x -> go (E sps) (mergeE (E is) e)
  go spawn (E []) = case nextE spawn of
    Nothing -> []
    Just (t,e,spawn') -> go spawn' e
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

sinkEIO :: E a -> (Time -> a -> IO ()) -> IO ()
sinkEIO e act = case nextE e of
  Nothing -> return ()
  Just (t,x,e') -> do
    now <- getCurrentGlobalTime
    when (now < t) (threadDelay (nanoToMicro (t - now)))
    act t x
    sinkEIO e' act

sourceEIO :: IO (Time,a) -> IO (E a)
sourceEIO pull = return (E (next ())) where
  next _ = unsafePerformIO $ do
    (t,x) <- pull
    return (pureTimeVar t x : next ())


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
  (t1,wr) <- newTimeVar
  let t2 = pureTimeVar (now + delay2) 'p'
  forkIO $ do
    threadDelay (floor (delay1 * 1000000))
    wr (now + delay1) '?'
  putStr "now = "
  print now
  print (soonerTimeVar t1 t2)

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

{-
test :: Int -> B Int
test n = accum n (fromListE [(1000,succ),(2000,succ),(3000,succ)])
-}


nanoToMicro nano = floor (nano * 1000000)

testSink = do
  let e = fromListE [(0,'a'),(1,'b'),(2,'c')]
  setGlobalProgramStartTime
  sinkEIO e $ \t c -> do
    print (t,c)

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
