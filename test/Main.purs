module Test.Main where

import Prelude

import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, invalid, isValid)
import Data.Variant (Variant, prj)
import HomeRunBall (class CheckRules, BeginsWith, ValidatedString, checkRules)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Type.Prelude (class RowToList, RProxy(..), SProxy(..))

onlyOnApples ::
     ValidatedString (beginsApple :: BeginsWith "Apple") String
  -> String
onlyOnApples _ = "U R COOL"

validOf :: forall a errors rules rl
   . RowToList rules rl
  => CheckRules rl errors rules a
  => RProxy rules
  -> a
  -> V (NonEmptyList (Variant errors)) a
validOf _ s = pure s

rules = RProxy :: RProxy (beginsApple :: BeginsWith "Apple")

expected :: V (NonEmptyList (Variant (beginsApple :: String))) String
expected = validOf rules "U R COOL"

main :: _
main = run [consoleReporter] do
  describe "purescript-home-run-ball" do
    it "works with valid string" do
      let
        checkedString = checkRules rules "AppleSDdf"
      isValid checkedString `shouldEqual` true
      expected `shouldEqual` (onlyOnApples <$> checkedString)

    it "works with invalid string" do
      let
        checkedString = checkRules rules "BananaeSDdf"
      isValid checkedString `shouldEqual` false
      invalid (pure $ Just "beginsApple") `shouldEqual` (map extractBeginsApple `lmap` checkedString)

extractBeginsApple ::
     Variant (beginsApple :: String)
  -> Maybe String
extractBeginsApple = prj (SProxy :: SProxy "beginsApple")
