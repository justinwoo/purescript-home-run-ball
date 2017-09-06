module HomeRunBall where

import Prelude

import Data.Char (toUpper)
import Data.Const (Const(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(Pattern), charAt, contains, stripPrefix, stripSuffix)
import Data.String as S
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol, class RowToList, Proxy(Proxy), RLProxy(RLProxy), RProxy, SProxy(SProxy), reflectSymbol)
import Type.Row (Cons, Nil, kind RowList)

-- | Check a string for validation rules provided by a row proxy and return a validation result
checkRules :: forall row errors rl
   . RowToList row rl
  => CheckRules rl errors row String
  => RProxy row
  -> String
  -> VS errors row
checkRules _ str = const (Const str) <$> checkRulesImpl (RLProxy :: RLProxy rl) str

-- | Rule for checking what the string begins with
data BeginsWith (s :: Symbol)

-- | Rule for checking what the string ends with
data EndsWith (s :: Symbol)

-- | Rule for checking what the string contains
data Contains (s :: Symbol)

-- | Rule for checking if the string is capitalized
data Capitalized

-- | Rule for checking if the string is all caps
data AllCaps

-- | Rule for checking if the string is all lowercase
data Lowercase

-- | Type alias for a validated string and its rules
type ValidatedString (rules :: # Type) = Const String (RProxy rules)

-- | Type alias for a string validation result, with a list of labels that failed validation
type VS errors rules = V (NonEmptyList (Variant errors)) (ValidatedString rules)

-- ValidateRule

class ValidateRule rule a where
  validateRuleImpl :: Proxy rule -> a -> Boolean

instance validateRuleBeginsWith ::
  ( IsSymbol prefix
  ) => ValidateRule (BeginsWith prefix) String where
  validateRuleImpl _ str =
    isJust $ stripPrefix (Pattern $ reflectSymbol (SProxy :: SProxy prefix)) str

instance validateRuleEndsWith ::
  ( IsSymbol suffix
  ) => ValidateRule (EndsWith suffix) String where
  validateRuleImpl _ str =
    isJust $ stripSuffix (Pattern $ reflectSymbol (SProxy :: SProxy suffix)) str

instance validateRuleContains ::
  ( IsSymbol thingy
  ) => ValidateRule (Contains thingy) String where
  validateRuleImpl _ str =
    contains (Pattern <<< reflectSymbol $ SProxy :: SProxy thingy) str

instance validateRuleCapitalized :: ValidateRule Capitalized String where
  validateRuleImpl _ str
    | Just head <- charAt 0 str = toUpper head == head
    | otherwise = false

instance validateRuleAllCaps :: ValidateRule AllCaps String where
  validateRuleImpl _ str = S.toUpper str == str

instance validateRuleLowercase :: ValidateRule Lowercase String where
  validateRuleImpl _ str = S.toLower str == str

-- CheckRules

class CheckRules (rl :: RowList) (errors :: # Type) (rules :: # Type) a
  | rl -> errors rules where
  checkRulesImpl :: RLProxy rl -> a -> V (NonEmptyList (Variant errors)) Unit

instance checkRulesCons ::
  ( IsSymbol name
  , CheckRules tail errors rules a
  , RowCons name String trash errors
  , ValidateRule ty a
  ) => CheckRules (Cons name ty tail) errors rules a where
  checkRulesImpl _ str = curr <> rest
    where
      curr
        | validateRuleImpl (Proxy :: Proxy ty) str = pure unit
        | otherwise
        , namep <- SProxy :: SProxy name
        , name <- reflectSymbol namep =
            invalid <<< pure $ inj namep name
      rest = checkRulesImpl (RLProxy :: RLProxy tail) str

instance checkRulesNil :: CheckRules Nil errors rules a where
  checkRulesImpl _ str = pure unit
