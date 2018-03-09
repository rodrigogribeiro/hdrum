{-#LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Language.HDrum
{-  (
    Instrument (..)
  , Step (..)
  , DrumPattern
  , Track
  , hits
  , rests
  , normalize
  , (.++) -- sequential composition
  , (.&)  -- single track creation
  , (.||) -- parallel track composition
  , (.*)  -- repetition operator
  , wf    -- well formedness
  , dur   -- duration
  , pfold -- fold operator for patterns
  , tfold -- fold operator for tracks
  ) -} where

import Control.Monad
import Prelude hiding (repeat)
import Test.QuickCheck

data Instrument = Clap | Cymbal | Cuica deriving (Eq, Ord, Show, Enum)

data Step = X | O deriving (Eq, Ord, Show)

data DrumPattern
  = Step :> DrumPattern | End Step
    deriving (Eq, Ord, Show)

pfold :: (Step -> b) -> (Step -> b -> b) -> DrumPattern -> b
pfold f _ (End s)  = f s
pfold f g (s :> d) = g s (pfold f g d)

(.++.) :: DrumPattern -> DrumPattern -> DrumPattern
(End s) .++. d' = s :> d'
(s :> d) .++. d' = s :> (d .++. d')

data Track
  = Instrument :& DrumPattern
  | Track :|| Track
  deriving (Eq, Ord, Show)

(.&) :: Instrument -> DrumPattern -> Track
(.&) = (:&)

tfold :: (Instrument -> DrumPattern -> b) ->
        (b -> b -> b) -> Track -> b
tfold f _ (i :& d) = f i d
tfold f g (t1 :|| t2) = g (tfold f g t1) (tfold f g t2)

-- duration

class Dur a where
  dur :: a -> Int 

instance Dur Step where
  dur _ = 1

instance Dur DrumPattern where
  dur = pfold (\ _ -> 1) (\ _ ac -> 1 + ac)

instance Dur Track where
  dur = tfold (\ _ -> dur) max

-- repeat

class Repeat a where
  type R a
  repeat :: Int -> a -> R a

instance Repeat Step where
  type R Step = DrumPattern
  repeat n x
      | n <= 1 = End x
      | otherwise = x :> repeat (n - 1) x

instance Repeat DrumPattern where
  type R DrumPattern = DrumPattern

  repeat n x
    | n <= 1 = x
    | otherwise = x .++. (repeat (n - 1) x)

instance Repeat Track where
  type R Track = Track
  repeat n = tfold (\ i -> (:&) i . repeat n) (:||)

rests :: Int -> DrumPattern
rests n = n .* O

hits :: Int -> DrumPattern
hits n = n .* X

(.*) :: Repeat a => Int -> a -> R a
n .* x = repeat n x

-- sequential composition

class Sequential a b where
  type S a b
  (.++) :: a -> b -> S a b

instance Sequential Step Step where
  type S Step Step = DrumPattern

  s .++ s' = s :> (End s')

instance Sequential Step DrumPattern where
  type S Step DrumPattern = DrumPattern

  s .++ d = (s :> d)

instance Sequential DrumPattern Step where
  type S DrumPattern Step = DrumPattern

  d .++ s = d .++. (End s)
 
instance Sequential DrumPattern DrumPattern where
  type S DrumPattern DrumPattern = DrumPattern

  d .++ d' = d .++. d'

instance Sequential Track Track where
  type S Track Track = Track

  t1 .++ t2 = sumTrack t1 t2

-- parallel composition

(.||) :: Track -> Track -> Track
(.||) = (:||)

-- well formedness

class WellFormed a where
  wf :: a -> Bool

instance WellFormed DrumPattern where
  wf _ = True

instance WellFormed Track where
  wf t = tfold (\ _ d -> dur d == dur t) (&&) t 

-- track sequential composition

tracks :: Track -> [(Instrument, DrumPattern)]
tracks = tfold (\ i d -> [(i,d)]) (++)

normalize :: Track -> Track
normalize t
  = normalize' t (dur t)
    where
      normalize' (i :& d) n
              | dur d < n = i :& (d .++. rests (n - dur d))
              | otherwise = i :& d
      normalize' (t1 :|| t2) n = (normalize' t1 n) :|| (normalize' t2 n) 
  
sumTrack :: Track -> Track -> Track
sumTrack t1 t2
  = foldr1 (:||) (map (uncurry (:&)) (same ++ diff1 ++ diff2))
  where
    tr1 = tracks (normalize t1)
    tr2 = tracks (normalize t2)
    same = [(i, d1 .++ d2) | (i, d1) <- tr1, (i',d2) <- tr2, i == i']
    diff1 = [(i, d1 .++ (rests (dur t2))) | (i,d1) <- tr1, i `notElem` (map fst tr2)]
    diff2 = [(i, (rests (dur t1)) .++ d2) | (i,d2) <- tr2, i `notElem` (map fst tr1)]

-- test instances

instance Arbitrary Instrument where
   arbitrary
     = oneof
          [
            return Clap
          , return Cymbal
          , return Cuica
          ]


instance Arbitrary Step where
  arbitrary
     = frequency
          [ (70, return X)
          , (30, return O)
          ]

genDrumPattern :: Int -> Gen DrumPattern
genDrumPattern n
  = toDrumPat <$> vectorOf n (arbitrary :: Gen Step)
    where
      toDrumPat [] = End O -- this is impossible
      toDrumPat [ x ] = End x
      toDrumPat (x : xs) = x :> toDrumPat xs

genTrack :: Int -> Int -> Gen Track
genTrack s n
  | s <= 1    = (:&) <$> arbitrary <*> arbitrary
  | otherwise = liftM2 (:||) (genTrack s2 n) (genTrack s2 n)
      where
        s2 = s - 1

instance Arbitrary DrumPattern where
  arbitrary
    = choose (1,3) >>= genDrumPattern

instance Arbitrary Track where
  arbitrary
    = do
        s <- choose (1,3)
        n <- choose (1,3)
        genTrack s n
