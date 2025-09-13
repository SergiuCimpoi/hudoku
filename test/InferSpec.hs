module InferSpec (spec) where

import qualified Data.Vector as V
import Infer

import Test.Hspec

spec :: Spec
spec = describe "digitsToCellIds" $ do
    it "assigns indices to candidates correctly" $ do
        let input =
                [ Candidates [1, 2]
                , Value 5
                , Candidates [2, 3]
                , Candidates [9]
                , Value 4
                ]
            expected =
                V.fromList
                    [ [0] -- candidate 1 appears at cell 0
                    , [2, 0] -- candidate 2 appears at cells 2 and 0
                    , [2] -- candidate 3 at cell 2
                    , [] -- candidate 4 nowhere as candidate
                    , [] -- candidate 5 nowhere as candidate
                    , [] -- candidate 6 nowhere
                    , [] -- candidate 7 nowhere
                    , [] -- candidate 8 nowhere
                    , [3] -- candidate 9 at cell 3
                    ]
        digitsToCellIds input `shouldBe` expected
