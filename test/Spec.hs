import Test.QuickCheck
import Language.HDrum


-- properties

-- 1. repeat related properties

repeatDurPatProp :: Property
repeatDurPatProp
  = forAll (suchThat (arbitrary :: Gen Int) (> 0)) (\ n -> forAll (arbitrary :: Gen DrumPattern) (\d -> n * dur d == dur (n .* d)))

repeatDurTrackProp :: Property 
repeatDurTrackProp
  = forAll (suchThat (arbitrary :: Gen Int) (> 0)) (\ n -> forAll (arbitrary :: Gen Track) (\d -> n * dur d == dur (n .* d)))

repeatPreservesWf :: Property
repeatPreservesWf
  = forAll (suchThat (arbitrary :: Gen Int) (> 0)) (\ n -> forAll (arbitrary :: Gen Track) (\ t -> wf t ==> wf (n .* t)))

-- 2. normalize implies well formedness

normalizeWf :: Property
normalizeWf
  = forAll (arbitrary :: Gen Track)(\ t -> wf (normalize t))

-- 2. sequential composition properties

seqTrackWf :: Property
seqTrackWf
  = forAll (arbitrary :: Gen Track)(\t1 -> forAll (arbitrary :: Gen Track) (\t2 -> wf (t1 .++ t2)))

seqTrackDur :: Property
seqTrackDur
  = forAll (arbitrary :: Gen Track)(\t1 -> forAll (arbitrary :: Gen Track) (\t2 -> dur t1 + dur t2 == dur (t1 .++ t2)))

main :: IO ()
main
  = do
      putStrLn "\nRepeat Pattern duration property"
      quickCheck repeatDurPatProp
      putStrLn "Repeat Track duration property"
      quickCheck repeatDurTrackProp
      putStrLn "Repeat Wellformedness"
      quickCheck repeatPreservesWf
      putStrLn "Normalize Wf"
      quickCheck normalizeWf
      putStrLn "Sequential composition track wf"
      quickCheck seqTrackWf
      putStrLn "Sequential compoistion track duration"
      quickCheck seqTrackDur

