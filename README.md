# Purescript Home Run Ball

[![Build Status](https://travis-ci.org/justinwoo/purescript-home-run-ball.svg)](https://travis-ci.org/justinwoo/purescript-home-run-ball)

A library for applying a row of rules for validation on a string.

![](http://i.imgur.com/VOYNDVW.png)

## Example

```hs
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
```
