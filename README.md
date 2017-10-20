# Purescript Home Run Ball

[![Build Status](https://travis-ci.org/justinwoo/purescript-home-run-ball.svg)](https://travis-ci.org/justinwoo/purescript-home-run-ball)

A library for applying a row of rules for validation on any values, returning the original data with the rules applied or a list of the rules that failed.

![](http://i.imgur.com/VOYNDVW.png)

See the [blog post](https://qiita.com/kimagure/items/eeb40541fc56b8dba2cc) about this library.

## Example

@joneshf made a cool demo [here](https://github.com/joneshf/purescript-home-run-ball-demo) showing off this library and [Sparkle](https://github.com/sharkdp/purescript-sparkle).

Here's some selected excerpts from the tests:

```hs
onlyOnApples ::
     ValidatedValue (beginsApple :: BeginsWith "Apple") String
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
```
