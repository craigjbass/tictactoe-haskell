module TicTacToe.SolverSpec (spec) where

import Test.Hspec
import TicTacToe.Solver

spec :: Spec
spec = do
  describe "board solver" $ do
    it "deals with no moves left" $ do
      let move = bestMove O [O, X, O
                            ,O, X, X
                            ,X, O, O]
      move `shouldBe` Nothing
    it "can win (top) row" $ do
      let move = bestMove O [O, O, N
                            ,N, N, N
                            ,N, N, N]
      move `shouldBe` Just 3
    it "can win (middle) row" $ do
      let move = bestMove O [N, N, N
                            ,O, O, N
                            ,N, N, N]
      move `shouldBe` Just 6
    it "can win (bottom) row" $ do
      let move = bestMove O [N, N, N
                            ,N, N, N
                            ,O, O, N]
      move `shouldBe` Just 9
    it "should prevent opposition from winning" $ do
      let move = bestMove O [O, N, N
                         ,X, X, N
                         ,O, N, N]
      move `shouldBe` Just 6
