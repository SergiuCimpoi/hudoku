module InferSpec (spec) where

import qualified Data.Vector as V
import Infer

import Test.Hspec

spec1 :: Spec
spec1 = describe "digitsToCellIds" $ do
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
                    [ [0]
                    , [2, 0]
                    , [2]
                    , []
                    , []
                    , []
                    , []
                    , []
                    , [3]
                    ]
        digitsToCellIds input `shouldBe` expected

spec2 :: Spec
spec2 = describe "reduceHiddenPairs" $ do
    it "reduction of hidden pairs" $ do
        let input =
                V.fromList
                    [ Candidates [3, 8]
                    , Value 2
                    , Candidates [3, 6]
                    , Candidates [3, 4, 6]
                    , Value 5
                    , Candidates [3, 4]
                    , Candidates [1, 7, 8]
                    , Value 9
                    , Candidates [1, 6, 7]
                    ]
            expected =
                V.fromList
                    [ Candidates [3, 8]
                    , Value 2
                    , Candidates [3, 6]
                    , Candidates [3, 4, 6]
                    , Value 5
                    , Candidates [3, 4]
                    , Candidates [1, 7]
                    , Value 9
                    , Candidates [1, 7]
                    ]
        reduceHiddenPairs input `shouldBe` expected
spec :: Spec
spec = do
    spec1
    spec2
