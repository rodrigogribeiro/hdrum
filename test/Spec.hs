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

-- 3. algebraic properties of drum patterns

idDrumPatLeft :: Property
idDrumPatLeft
  = forAll (arbitrary :: Gen DrumPattern)
           (\ d -> d .++ (fromList []) == d)

idDrumPatRight :: Property
idDrumPatRight
  = forAll (arbitrary :: Gen DrumPattern)
           (\ d -> (fromList []) .++ d == d)
 

drumPatAssoc :: Property
drumPatAssoc
  = forAll (arbitrary :: Gen DrumPattern)
           (\ d1 -> forAll (arbitrary :: Gen DrumPattern)
                           (\ d2 -> forAll (arbitrary :: Gen DrumPattern)
                                           (\ d3 -> let
                                                     x = ((d1 .++ d2) .++ d3) :: DrumPattern
                                                     y = (d1 .++ (d2 .++ d3)) :: DrumPattern
                                                    in x == y)))

-- 4. algebraic properties of tracks

instruments :: Track -> [Instrument]
instruments = tfold (\ i _ -> [i]) (++)

idTrackLeft :: Property
idTrackLeft
  = forAll (arbitrary :: Gen Track)
           (\ t -> forAll (elements (instruments t))
                          (\ i -> t .++ (i .& mempty) == t))


idTrackRight :: Property
idTrackRight
  = forAll (arbitrary :: Gen Track)
           (\ t -> forAll (elements (instruments t))
                          (\ i -> (i .& mempty) .++ t == t))


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
      putStrLn "Sequential composition track duration"
      quickCheck seqTrackDur
      putStrLn "Empty pattern identity left"
      quickCheck idDrumPatLeft
      putStrLn "Empty pattern identity right"
      quickCheck idDrumPatRight
      putStrLn "Drum pattern sequential composition is associative"
      quickCheck drumPatAssoc
      putStrLn "Identity for sequential composition of tracks left"
      quickCheck idTrackLeft
      putStrLn "Identity for sequential composition of tracks right"
      quickCheck idTrackRight

