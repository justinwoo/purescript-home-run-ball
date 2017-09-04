module Test.Main where

import Prelude

import Data.Validation.Semigroup (invalid, isValid)
import HomeRunBall (BeginsWith, ValidatedString, checkRules)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Type.Prelude (RProxy(..))

onlyOnApples :: forall r
   . ValidatedString (beginsApple :: BeginsWith "Apple" | r)
  -> String
onlyOnApples _ = "U R COOL"

main :: _
main = run [consoleReporter] do
  describe "purescript-home-run-ball" do
    it "works with valid string" do
      let
        rules = RProxy :: RProxy (beginsApple :: BeginsWith "Apple")
        checkedString = checkRules rules "AppleSDdf"
      isValid checkedString `shouldEqual` true
      pure "U R COOL" `shouldEqual` (onlyOnApples <$> checkedString)

    it "works with invalid string" do
      let
        rules = RProxy :: RProxy (beginsApple :: BeginsWith "Apple")
        checkedString = checkRules rules "BananaeSDdf"
      isValid checkedString `shouldEqual` false
      invalid (pure "beginsApple") `shouldEqual` checkedString
