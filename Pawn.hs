{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Pawn where

import Data.Stream (Stream)
import qualified Data.Stream as Stream

import Varying (Varying, current); import qualified Varying
import Sequence (Sequence, periodic); import qualified Sequence

newtype M i o = M { runM :: i -> (o, M i o) }
data StepOr r i = Step r | Poke r i
data P a b = P !a !b deriving Show
fst' (P x y) = x
snd' (P x y) = y
data DirWE = East | West deriving Show

type PlatformID = Integer
data Platform = Platform
  { platID       :: !PlatformID
  , platY        :: !Integer
  , platMaterial :: !() } deriving Show

data Stick = StickLeft | StickNeutral | StickRight deriving Show
data Support = NoFloor | YesFloor !Platform deriving Show
data VerticalStatus = FreeFall !Integer !Rational | StandingOn !Platform
  deriving Show

data Motion = Mo
  { moXDX :: !(P Integer Rational)
  , moYDY :: !VerticalStatus
  , moDir :: DirWE
  } deriving Show

data AbstractSprite =
  Standing |
  Walking Int |
  Stopping |
  Jumping |
  Falling
    deriving Show

data AbstractSound =
  StepSound |
  JumpSound |
  ScreechSound
    deriving Show

data Action =
  Jump |
  CancelJump |
  StickChange Stick |
  Bump Force |
  Stomped |
  HitCeiling |
  Earthquake
    deriving Show

type PawnEnv = (Stick, Support)
type PawnStatus = (Motion, AbstractSprite)
type PawnOut = (PawnStatus, [AbstractSound])
type Pawn = M (StepOr PawnEnv Action) PawnOut
data Next = Next Motion AbstractSprite Pawn

type XLoc = Integer
type YLoc = Integer
type XVel = Rational
type YVel = Rational
type Force = (Integer,Integer)
type Dynamics1D = Varying (P Integer Rational)
type Animation = Varying (Stream AbstractSprite)
look :: Animation -> AbstractSprite
look = Stream.head . current
pos :: Dynamics1D -> Integer
pos = fst' . current
speed :: Dynamics1D -> Rational
speed = abs . snd' . current
isPrettySlow = (< 5)
fromDynamics :: Dynamics1D -> VerticalStatus
fromDynamics d = let (P x v) = current d in FreeFall x v
vertY :: VerticalStatus -> Integer
vertY (FreeFall y _) = y
vertY (StandingOn plat) = platY plat
slowPath :: XLoc -> XVel -> Dynamics1D
slowPath x0 v0 = pure (P x0 v0)
slowDownVarying :: Varying a -> Varying a
slowDownVarying = id
slowDownSequence :: Sequence a -> Sequence a
slowDownSequence = id

-- adjust!
newWalkingPath :: XLoc -> XVel -> DirWE -> Dynamics1D
newWalkingPath x0 v0 dir = pure (P x0 v0)

-- adjust!
newWalkingAnimation :: Animation
newWalkingAnimation = pure (Stream.repeat (Walking 0))

-- adjust!
newWalkingSoundSequence :: Sequence ()
newWalkingSoundSequence = periodic 25 ()

-- adjust!
freeFallPath :: YLoc -> YVel -> Dynamics1D
freeFallPath y0 vy0 = pure (P y0 vy0)

-- adjust
uniformMotion :: XLoc -> XVel -> Dynamics1D
uniformMotion x0 vx0 = pure (P x0 vx0)

runningSpeed = 5

next :: Next -> (PawnOut, Pawn)
next (Next mo pic k) = (((mo, pic), []), k)

nextSnd :: AbstractSound -> Next -> ((PawnStatus,[AbstractSound]), Pawn)
nextSnd snd (Next mo pic k) = (((mo, pic), [snd]), k)

stationary :: XLoc -> DirWE -> Platform -> Next
stationary x dir plat = Next motion Standing go where
  motion = Mo (P x 0) (StandingOn plat) dir
  noEffect = next (stationary x dir plat)
  go = M $ \case
    Step (_,floor) -> case floor of
      YesFloor plat' -> next (stationary x dir plat')
      NoFloor        -> next (startFalling motion 0)
    Poke _ (StickChange stick) -> case stick of
      StickLeft  -> next (startWalking x 0 West plat)
      StickRight -> next (startWalking x 0 East plat)
      _          -> noEffect
    Poke _ Jump         -> nextSnd JumpSound (startJumping motion)
    Poke _ (Bump force) -> next (getBumpedOnLand motion plat force)
    Poke _ Stomped      -> next (startBeingStomped motion plat)
    Poke _ Earthquake   -> next (startQuaking motion plat)
    _                   -> noEffect

startWalking :: XLoc -> XVel -> DirWE -> Platform -> Next
startWalking x vx dir plat = walking x' dir gfx feet plat where
  x' = newWalkingPath x vx dir 
  gfx = newWalkingAnimation
  feet = newWalkingSoundSequence

walking :: Dynamics1D -> DirWE -> Animation -> Sequence () -> Platform -> Next
walking x dir anim feet plat = Next motion (look anim) go where
  motion = Mo (current x) (StandingOn plat) dir
  go = M $ \case
    Step (_,floor) ->
      let !x' = Varying.step x in
      let !anim' = Varying.step anim in
      case floor of
        YesFloor plat' -> case Sequence.step feet of
          Left feet'      -> next (walking x' dir anim' feet' plat')
          Right ((), feet') -> nextSnd StepSound (walking x' dir anim' feet' plat')
        NoFloor ->
          let !motion' = motion { moXDX = current x', moYDY = FreeFall (platY plat) 0 }
          in next (startFalling motion' 0)
    Poke _ (StickChange _)
      | isPrettySlow (speed x) -> next (startSlowing motion plat anim feet)
      | otherwise              -> startStopping motion plat
    Poke _ Jump         -> nextSnd JumpSound (startJumping motion)
    Poke _ (Bump force) -> next (getBumpedOnLand motion plat force)
    Poke _ Stomped      -> next (startBeingStomped motion plat)
    Poke _ Earthquake   -> next (startQuaking motion plat)

startFalling :: Motion -> YVel -> Next
startFalling (Mo (P x0 vx0) y0 dir) vy0 = falling x y dir where
  x = uniformMotion x0 vx0
  y = freeFallPath (vertY y0) vy0

falling :: Dynamics1D -> Dynamics1D -> DirWE -> Next
falling x y dir = Next motion Falling go where
  motion = Mo (current x) (fromDynamics y) dir
  noEffect = next (falling x y dir)
  go = M $ \case
    Step (stick,floor) -> case floor of
      NoFloor ->
        let x' = Varying.step x in
        let y' = Varying.step y in
        next (falling x' y' dir)
      YesFloor plat -> executeLanding motion plat stick
    Poke _ (Bump force) -> next (getBumpedInAir motion force)
    _                   -> noEffect

getBumpedOnLand :: Motion -> Platform -> Force -> Next
getBumpedOnLand motion@(Mo x y dir) plat force = undefined

getBumpedInAir :: Motion -> Force -> Next
getBumpedInAir motion@(Mo x y dir) force = undefined

executeLanding :: Motion -> Platform -> Stick -> (PawnOut, Pawn)
executeLanding mo@(Mo (P x vx) _ dir) plat stick = go where
  backpedal = undefined
  comeToRest = slowing (slowPath x vx) dir restAnim restSeq plat
  restAnim = slowDownVarying newWalkingAnimation
  restSeq = slowDownSequence newWalkingSoundSequence
  go = case stick of
    StickNeutral | vx == 0               -> next (stationary x dir plat)
                 | abs vx < runningSpeed -> next comeToRest
                 | otherwise             -> startStopping mo plat
    StickLeft    | vx <= 0               -> next (startWalking x vx dir plat)
                 | vx > 0                -> startStopping mo plat
    StickRight   | vx >= 0               -> next (startWalking x vx dir plat)
                 | vx < 0                -> startStopping mo plat

startSlowing :: Motion -> Platform -> Animation -> Sequence () -> Next
startSlowing (Mo x0 y0 dir) plat anim feet = slowing x dir anim' feet' plat where
  x = undefined
  anim' = undefined
  feet' = undefined

slowing :: Dynamics1D -> DirWE -> Animation -> Sequence () -> Platform -> Next
slowing x dir anim feet plat = Next motion (look anim) go where
  motion = Mo (current x) (StandingOn plat) dir
  go = M $ \case
    Step (stick,floor) ->
      let !x' = Varying.step x in
      let !anim' = Varying.step anim in
      case floor of
        YesFloor plat' -> if speed x' == 0
          then case stick of
            StickLeft    -> next (startWalking (pos x') 0 West plat')
            StickRight   -> next (startWalking (pos x') 0 East plat')
            StickNeutral -> next (stationary (pos x') dir plat')
          else case Sequence.step feet of
            Left feet'        -> next (slowing x' dir anim' feet' plat')
            Right ((), feet') -> nextSnd StepSound (slowing x' dir anim' feet' plat')
        NoFloor ->
          let !motion' = motion { moXDX = current x' , moYDY = FreeFall (platY plat) 0 }
          in next (startFalling motion' 0)
    Poke _ Jump         -> nextSnd JumpSound (startJumping motion)
    Poke _ (Bump force) -> next (getBumpedOnLand motion plat force)
    Poke _ Stomped      -> next (startBeingStomped motion plat)
    Poke _ Earthquake   -> next (startQuaking motion plat)
    _ -> next (slowing x dir anim feet plat)

startStopping :: Motion -> Platform -> (PawnOut, Pawn)
startStopping (Mo (P x0 vx0) _ dir) plat = nextSnd ScreechSound go where
  go = stopping (slowPath x0 vx0) dir plat

stopping :: Dynamics1D -> DirWE -> Platform -> Next
stopping x dir plat = Next motion Stopping go where
  motion = Mo (current x) (StandingOn plat) dir
  go = M $ \case
    Step (stick,floor) ->
      let !x' = Varying.step x in
      case floor of
        YesFloor plat' -> if speed x' == 0
          then case stick of
            StickLeft    -> next (startWalking (pos x') 0 West plat')
            StickRight   -> next (startWalking (pos x') 0 East plat')
            StickNeutral -> next (stationary (pos x') dir plat')
          else next (stopping x' dir plat')
        NoFloor ->
          let !motion' = motion { moXDX = current x' , moYDY = FreeFall (platY plat) 0 }
          in next (startFalling motion' 0)
    Poke _ Jump         -> nextSnd JumpSound (startJumping motion)
    Poke _ (Bump force) -> next (getBumpedOnLand motion plat force)
    Poke _ Stomped      -> next (startBeingStomped motion plat)
    Poke _ Earthquake   -> next (startQuaking motion plat)
    _ -> next (stopping x dir plat)

startJumping :: Motion -> Next
startJumping motion@(Mo x y dir) = undefined

startBeingStomped :: Motion -> Platform -> Next
startBeingStomped motion@(Mo x y dir) plat = undefined

startQuaking :: Motion -> Platform -> Next
startQuaking motion@(Mo x y dir) plat = undefined

{-

jumping :: Dynamics1D -> Dynamics1D -> Direction -> AbstractSprite -> Next
jumping x y dir pic = Next motion pic go where
  motion = Mo (current x) (Falling (pos y) (vel y)) dir
  noEffect = next (jumping x y dir pic)
  go = M $ \case
    Step (_,floor) ->
      let x' = stepVarying x in
      let y' = stepVarying y in
      case floor of
        YesFloor plat -> next (landOnPlatform x' y' dir plat)
        NoFloor       -> next (jumping x' y' dir pic)
    Poke _ CancelJump
      | velocity y > 0 -> next (jumping x cancelledJumpPath dir pic)
      | otherwise      -> noEffect
    Poke _ HitCeiling   -> next (falling x hitCeilingPath dir pic)
    Poke _ (Bump force) -> next (getBumpedInAir (pos x) (pos y) force)
    _ -> noEffect

startWalking :: Direction -> XLoc -> Platform -> Next
startWalking dir x0 plat0 =
  walking (newWalkingPath x0 0 dir) walkingGfx stepSequence plat0

-}

{-

data LandData = LandData
  { landX :: !Dynamics1D
  , landDir :: !DirWE
  , landPlat :: !Platform
  , landAnim :: !Animation
  , landFeet :: !Footsteps
  , landDisabledTimeLeft :: !Int
  , landStep :: LandData -> Stick -> Support -> Next
  , landCtrl :: LandData -> Stick -> Support -> Next
  }

data AirData :: AirData
  { airX :: !Dynamics1D
  , airY :: !Dynamics1D
  , airDir :: !DirWE
  , airPic :: !AbstractSprite
  }

land :: LandData -> Next
land ld@(LandData x dir plat anim feet disTimer step) = Next motion pic go where
  pic = look anim
  motion = Mo (current x) (StandingOn plat) dir
  noEffect = next (onLand ld)
  go = M $ \case
    Step (stick,floor)  -> 
      let !x' = Varying.step x in
      let !anim' = Varying.step anim in
      case floor of
        YesFloor plat' -> if speed x == 0
          then case stick of
            StickLeft    -> next (startWalking (pos x') 0 West plat')
            StickRight   -> next (startWalking (pos x') 0 East plat')
            StickNeutral -> next (stationary (pos x') dir plat')
          else case Sequence.step feet of
            Left feet'        -> next (slowing x' dir anim' feet' plat')
            Right ((), feet') -> nextSnd StepSound (slowing x' dir anim' feet' plat')
        NoFloor ->
          let !y' = freeFallPath (platY plat) 0 in
          let !airData = AirData x' y' dir Falling in
          next (air airData)
    Poke _ Jump
      | disTimer > 0    -> noEffect
      | otherwise       -> nextSnd JumpSound (startJumping motion)
    Poke (_,floor) (StickChange stick)
      | disTimer > 0    -> noEffect
      | otherwise       -> ctrl ld stick floor
    Poke _ (Bump (force,_)) -> 
      let x' = slowingPath force in
      let anim' = pure Stopping in
      nextSnd ScreechSound (land (LandData x' dir plat anim' Ended 0 slipping))
    Poke _ Stomped      -> 
      let x' = pure (P (pos x) 0) in
      let anim' = pure Stuck in
      next (land (LandData x' dir plat anim' Ended 50 stuck))
    Poke _ Earthquake   -> 
      let x' = pure (P (pos x) 0) in
      let anim' = shaking Stopped in
      next (land (LandData x' dir plat anim' Ended 50 stuck))
    _ -> noEffect

air :: AirData -> Next
air ad@(AirData x y dir pic) = Next motion pic go where
  motion = Mo (current x) (fromDynamics y) dir
  go = M $ \case
    Step (stick,floor) -> case floor of
      NoFloor ->
        let x' = Varying.step x in
        let y' = Varying.step y in
        next (air (AirData x' y' dir pic))
      YesFloor plat -> executeLanding motion plat stick
    Poke _ (Bump force) -> next (getBumpedInAir motion force)
    _                   -> noEffect
  
type PawnWalk = M Stick (XLoc, XVel, Dir, AbstractSprite, [AbstractSound])
data Next = Next XLoc XVel Dir AbstractSprite [AbstractSound] PawnWalk

pawnWalk :: XLoc -> XVel -> Dir -> PawnWalk
pawnWalk x vx dir pic = Next x vs dir pic [] (go ) where
  state0 = M $ \case
    StickNeutral -> -- move speed toward zero
    StickLeft    -> -- if speed is left or zero and less than max, increase left
                    -- if speed is right, go to state 1
    StickRight   -> 
  state1 stick = -- speed moves toward zero, controls dont respond
                 -- when you reach zero, go back to state 0

-}

newStepPulseGen :: M Bool Bool
newStepPulseGen =
  newRampGen 0 50000 1 `comp` newPulseGen 0

newRampGen :: Integer -> Integer -> M Bool Integer
newRampGen cMin cMax = go 0 0 where
  d = 
  go c acc = M (f c acc)
  f c acc i
    | c >= cMax = (cMax, go cMax acc)
    | c <= cMin = (cMin, go cMin acc)
    | otherwise = let (q,r) = divMod (acc + d) accMax in
                  let c' = if i then c + q else c - q in
                  (c, go c' r)
    
newPulseGen :: Integer -> M Integer Bool
newPulseGen c0 = go c0 where
  cMax = 1000000
  go c = M f
  f 0 = (False, go c)
  f i = let (q,r) = (c + i) `divMod` cMax in (q > 0, go r)

iterateM :: M i o -> [i] -> [o]
iterateM _ [] = []
iterateM (M f) (i:is) = let (o,m') = f i in o : iterateM m' is

newBoundedAccum :: Ord a -> a -> a -> a -> M (a -> a) a
newBoundedAccum cMin cMax c0 = go c0 where
  go c = M $ \f -> let !c' = f c in
    if | c' < cMin -> (cMin, go cMin)
       | c' > cMax -> (cMax, go cMax)
       | otherwise -> (c', go c')
