{-# LANGUAGE DeriveFunctor #-}
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

type UnsafeTimeFunc a = Time -> (CellStatus a, [(Time,a)])

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

--data R a =
--  RPure (CellStatus a) | RCons (CellStatus a) (TimeVar (R a))

newtype E a = E [TimeVar a]
newtype B a = B { unsafeSeek :: UnsafeTimeFunc a }

instance Monoid a => Monoid (E a) where
  mempty = neverE
  mappend = mergeE

instance Functor E where
  fmap f e = E (go e) where
    go e = case nextE e of
      Nothing -> []
      Just (t,x,e') -> pureTimeVar t (f x) : go e'

instance Functor B where
  fmap f b = B seek' where
    seek' t = (fmap f st, map (fmap f) updates) where
      (st,updates) = unsafeSeek b t

instance Applicative B where
  pure x = B (\t -> (Initial x, []))
  bf <*> bx = B seek' where
    seek' t = (st', updates') where
      (stf, upf) = unsafeSeek bf t
      (stx, upx) = unsafeSeek bx t
      st' = stf <*> stx
      f0 = cellValuePlus stf
      x0 = cellValuePlus stx
      updates' = timeZip ($) f0 upf x0 upx

data Sooner a b =
  AWins Time a |
  BWins Time b |
  ABTie Time a b
    deriving Show

timeZip :: (a -> b -> c) -> a -> [(Time,a)] -> b -> [(Time,b)] -> [(Time,c)]
timeZip f x0 arg1@((t1,x):xs) y0 arg2@((t2,y):ys)
  | t1 < t2 = (t1, f x y0) : timeZip f x xs y0 arg2
  | t2 < t1 = (t2, f x0 y) : timeZip f x0 arg1 y ys
  | otherwise = (t1, f x y) : timeZip f x xs y ys
timeZip _ _ _ _ _ = []

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

at :: B a -> Time -> a
at (B seek) t = case seek t of
  (st, _) -> cellValueMinus st

seekCellTo :: CellGuts a -> Time -> IO [(Time,a)]
seekCellTo (vv,ve) t = begin where
  begin = do
    v <- readIORef vv
    when (t `lessThanLastCellTime` v) (throwIO (TimeError 1))
    backwards <- collect []
    return (reverse backwards)
  collect changes = do
    e <- readIORef ve
    v <- readIORef vv
    case e of
      E [] -> return changes
      E (i:is) -> case soonerTimeVar (pureTimeVar t undefined) i of
        AWins _ _ -> do
          writeIORef vv (Dormant t (cellValuePlus v))
          return changes
        BWins te _ -> do
          let Just (_, f, e') = nextE e
          let x = cellValuePlus v
          let x' = f x
          writeIORef vv (Dormant te x')
          writeIORef ve e'
          collect ((te,x'):changes)
        ABTie _ _ _ -> do
          let Just (_, f, e') = nextE e
          let x = cellValuePlus v
          let x' = f x
          writeIORef vv (Transit t x x')
          writeIORef ve e'
          return ((t,x'):changes)

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
  vv0 <- newIORef (Initial x0)
  ve0 <- newIORef e0
  anchor <- newMVar (vv0,ve0)
  w <- mkWeakMVar anchor (return ())
  ch <- dupChan globalHousekeepingChan
  forkIO (cellKludgeThread w ch)
  return $ B $ \t -> unsafePerformIO $ withMVar anchor $ \guts@(vv,ve) -> do
    v <- readIORef vv
    updates <- seekCellTo guts t
    return (v,updates)

hmm n = do
  let b = test n
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
  hmm3 30

hmm3 0 = return ()
hmm3 n = do
  threadDelay 1000000
  writeChan globalHousekeepingChan (60 - n)
  print ("done " ++ show n)
  hmm3 (n-1)

test n = accum n (fromListE [(1000,succ),(2000,succ),(3000,succ)])

cellKludgeThread ::
  Weak (MVar (CellGuts a)) -> Chan Time -> IO ()
cellKludgeThread w houseKeeping = loop where
  loop = do
    now <- readChan houseKeeping
    r <- deRefWeak w
    case r of
      Nothing -> do
        threadDelay 1234
        print "No more object, I go poof"
      Just mv -> do
        print now
        print "checking..."
        withMVar mv $ \guts@(vv,ve) -> do
          v <- readIORef vv
          case v of
            Initial _ -> print "initial."
            Dormant t _ -> print ("dormant " ++ show t)
            Transit t _ _ -> print ("transit " ++ show t)
          when (now `greaterThanLastCellTime` v) $ do
            seekCellTo guts now
            print "seeked."
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

{-
switcher :: B a -> E (B a) -> B a
switcher b0 e = B seek' where
  seek' t = at (fmap (g t) v) t where
  g t b = at b t
  v = accum b0 (fmap const e)
-}

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

{-
b3 :: B Char
b3 = switcher b1 es2
-}

b4 :: B Integer
b4 = accum 0 (fromListE $ zip [1,2..] (repeat (succ)))

es3 = fromListE (zip [10,20..] (cycle [pure 0, b4]))

--b5 = switcher b4 es3

pulses = fromListE (zip [0,1..] (repeat ()))
