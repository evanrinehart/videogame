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
data TimeVar a = TimeVar 
  { readTimeVar :: (Time,a)
  , writeTimeVar :: Time -> a -> IO ()
  , tryReadTimeVar :: STM (Maybe (Time,a)) }
newtype E a = E [TimeVar a]
newtype B a = B (Time -> a)

instance Monoid a => Monoid (E a) where
  mempty = neverE
  mappend = mergeE

instance Functor E where
  fmap f e = E (go e) where
    go e = case nextE e of
      Nothing -> []
      Just (t,x,e') -> pureTimeVar t (f x) : go e'

instance Functor B where
  fmap f b = B (\t -> f (at b t))

instance Applicative B where
  pure x = B (\t -> x)
  bf <*> bx = B $ \t -> (at bf t) (at bx t)

data Sooner a b =
  AWins Time a |
  BWins Time b |
  ABTie Time a b
    deriving Show

newTimeVar :: IO (TimeVar a)
newTimeVar = do
  mv <- newEmptyTMVarIO
  return $ TimeVar
    (unsafePerformIO $ atomically (readTMVar mv))
    (\t x -> atomically (tryPutTMVar mv (t,x) >> return ()))
    (tryReadTMVar mv)

pureTimeVar :: Time -> a -> TimeVar a
pureTimeVar t x = TimeVar
  (t,x)
  (\_ _ -> return ())
  (return (Just (t,x)))

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

nextE :: E a -> Maybe (Time, a, E a)
nextE (E []) = Nothing
nextE (E (i:is)) =
  let (t,x) = readTimeVar i in
  Just (t, x, E is)

instance Show a => Show (E a) where
  show (E is) = show (map readTimeVar is)

data AccumStatus a =
  Dormant Time a |
  Transit Time a a |
  Initial a
    deriving Show

cellTimeLT :: Time -> IORef (AccumStatus a) -> IO Bool
cellTimeLT t ref = do
  st <- readIORef ref
  case st of
    Dormant t' _ -> return (t' < t)
    Transit t' _ _ -> return (t' < t)
    Initial _ -> return True

at :: B a -> Time -> a
at (B f) t = f t

magicAccumFunc ::
  IORef (AccumStatus a) -> IORef (E (a -> a)) -> Time -> a
magicAccumFunc vv ve t =
  unsafePerformIO $ fix $ \loop -> do
    mr <- do
      let err n = throwIO (TimeError n)
      e <- readIORef ve
      case e of
        E [] -> do
          v <- readIORef vv
          case v of
            Dormant t' x
              | t >= t' -> return (Just x)
              | otherwise -> err 1
            Transit t' x0 x1
              | t == t' -> return (Just x0)
              | t > t' -> return (Just x1)
              | otherwise -> err 2
            Initial x -> return (Just x)
        E (i:is) -> case soonerTimeVar (pureTimeVar t undefined) i of
          AWins _ _ -> do
            v <- readIORef vv
            case v of
              Dormant t' x
                | t >= t' -> return (Just x)
                | otherwise -> err 3
              Transit t' x0 x1
                | t > t' -> do
                    writeIORef vv (Dormant t x1)
                    return (Just x1)
                | t == t' -> return (Just x0)
                | otherwise -> err 4
              Initial x -> return (Just x)
          BWins te _ -> do
            let Just (_, f, e') = nextE e
            v <- readIORef vv
            let arg = case v of
                  Dormant t' x -> x
                  Transit t' x0 x1 -> x1
                  Initial x -> x
            writeIORef vv (Dormant te (f arg))
            writeIORef ve e'
            return Nothing
          ABTie _ _ _ -> do
            let Just (_, f, e') = nextE e
            v <- readIORef vv
            case v of
              Dormant t' x -> do
                when (t < t') (err 5)
                writeIORef vv (Transit t x (f x))
                writeIORef ve e'
                return (Just x)
              Transit t' x0 x1 -> do
                when (t < t') (err 6)
                writeIORef vv (Transit t x1 (f x1))
                writeIORef ve e'
                return (Just x1)
              Initial x -> do
                writeIORef vv (Transit t x (f x))
                writeIORef ve e'
                return (Just x)
            return Nothing
    case mr of
      Nothing -> loop
      Just ans -> return ans

data TimeError = TimeError Int deriving Show

instance Exception TimeError

loopTest :: B Char
loopTest = n2 where
  n2 = accum 'a' n1
  n1 = snapshot e n2
  e = fromListE [(1,const succ),(2,const succ),(3,const succ)]

runTest :: Nano -> Char -> Nano -> Char -> IO ()
runTest delay1 c1 delay2 c2 = do
  now <- getCurrentGlobalTime
  t1 <- newTimeVar
  let t2 = pureTimeVar (now + delay2) 'p'
  forkIO $ do
    threadDelay (floor (delay1 * 1000000))
    writeTimeVar t1 (now + delay1) '?'
  putStr "now = "
  print now
  print (soonerTimeVar t1 t2)

fromListE :: [(Time,a)] -> E a
fromListE xs = E (go xs) where
  go [] = []
  go ((t,x):rest) = pureTimeVar t x : go rest

fromFunctionB :: (Time -> a) -> B a
fromFunctionB f = B f

timeShiftB :: Time -> B a -> B a
timeShiftB dt (B f) = B (f . (subtract dt))

mergeE :: Monoid a => E a -> E a -> E a
mergeE (E xs) (E ys) = E (go xs ys) where
  go e1@(i:is) e2@(j:js) = case soonerTimeVar i j of
    AWins t x -> pureTimeVar t x : go is e2
    BWins t x -> pureTimeVar t x : go e1 js
    ABTie t x y -> pureTimeVar t (mappend x y) : go is js
  go is [] = is
  go [] js = js

neverE :: E a
neverE = E []

accum :: a -> E (a -> a) -> B a
accum x0 e0 = unsafePerformIO $ do
  vv <- newIORef (Initial x0)
  ve <- newIORef e0
  let magic = magicAccumFunc vv ve
  anchor <- newMVar (vv,magic)
  w <- mkWeakMVar anchor (return ())
  ch <- dupChan globalHousekeepingChan
  forkIO (accumKludgeThread w ch)
  return $ B $ \t -> unsafePerformIO $ withMVar anchor $ \(_,magic) -> do
    return (magic t)

hmm n = do
  let b = test n
  hmm2 b

hmm2 b = do
  threadDelay 1000000
  writeChan globalHousekeepingChan 3
  print (at b 3)
  threadDelay 1000000
  writeChan globalHousekeepingChan 4
  print (at b 4)
  threadDelay 1000000
  writeChan globalHousekeepingChan 5
  print (at b 5)
  threadDelay 1000000
  writeChan globalHousekeepingChan 6
  print (at b 6)
  threadDelay 1000000
  writeChan globalHousekeepingChan 7
  print (at b 7)
  threadDelay 1000000
  writeChan globalHousekeepingChan 8
  print (at b 8)
  hmm3 30

hmm3 0 = return ()
hmm3 n = do
  threadDelay 1000000
  writeChan globalHousekeepingChan (60 - n)
  print ("done " ++ show n)
  hmm3 (n-1)

test n = accum n (fromListE [(1000,succ),(2000,succ),(3000,succ)])

accumKludgeThread ::
  Weak (MVar (IORef (AccumStatus a), Time -> a)) -> Chan Time -> IO ()
accumKludgeThread w houseKeeping = loop where
  loop = do
    now <- readChan houseKeeping
    r <- deRefWeak w
    case r of
      Nothing -> do
        print "No magic container, I go poof"
      Just mv -> do
        withMVar mv $ \(ref,magicSeek) -> do
          old <- cellTimeLT now ref
          when old $ do
            let x = magicSeek now
            evaluate x
            print "evaluated."
            return ()
        loop

snapshot :: E (a -> b) -> B a -> E b
snapshot e b = E (go e) where
  go (E []) = []
  go e = case nextE e of
    Nothing -> []
    Just (t,f,e') ->
      let x = at b t in
      pureTimeVar t (f x) : go e'

snapshot_ :: E b -> B a -> E a
snapshot_ e b = snapshot (fmap (\_ -> id) e) b

switcher :: B a -> E (B a) -> B a
switcher b0 e = B f where
  f t = at (fmap (g t) v) t
  g t b = at b t
  v = accum b0 (fmap const e)

justE :: E (Maybe a) -> E a
justE e = E (go e) where
  go (E []) = []
  go e = case nextE e of
    Nothing -> []
    Just (t,Nothing,e') -> go e'
    Just (t,Just x, e') -> pureTimeVar t x : go e'

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

-- when the event occurs, shift the payload event up to now.
modernizeE :: E (E a) -> E (E a)
modernizeE es = mapWithTimeE timeShiftE es

--barn :: E (E k, B a) -> (B [a], B BarnStats)

sinkEIO :: E a -> (Time -> a -> IO ()) -> IO ()
sinkEIO e act = case nextE e of
  Nothing -> return ()
  Just (t,x,e') -> do
    now <- getCurrentGlobalTime
    when (now < t) (threadDelay (nanoToMicro (t - now)))
    act t x
    sinkEIO e' act

nanoToMicro nano = floor (nano * 1000000)

testSink = do
  let e = fromListE [(0,'a'),(1,'b'),(2,'c')]
  setGlobalProgramStartTime
  sinkEIO e $ \t c -> do
    print (t,c)

b1 :: B Char
b1 = pure 'a'

b2 :: B Char
b2 = accum 'x' es

es :: E (Char -> Char)
es = fromListE (zip [5,6..] (cycle [const 'y', const 'z']))

es2 :: E (B Char)
es2 = fromListE (zip [10,20..] (cycle [b1, b2]))

b3 :: B Char
b3 = switcher b1 es2

b4 :: B Integer
b4 = accum 0 (fromListE $ zip [1,2..] (repeat (succ)))

es3 = fromListE (zip [10,20..] (cycle [pure 0, b4]))

b5 = switcher b4 es3

pulses = fromListE (zip [0,1..] (repeat ()))
