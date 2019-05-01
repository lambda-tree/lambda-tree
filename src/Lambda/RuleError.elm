module Lambda.RuleError exposing (..)


ctxSame =
    "Top & bottom contexts are not same"


termSame =
    "Top & bottom terms are not same"


tySame =
    "Top & bottom types are not same"


varTyCtx =
    "Type of variable in context is different"


varSpec =
    "Type of variable is incorrectly specialized"


varNotInCtx =
    "Variable is not bound in context"


tyBool =
    "Type is not Bool"


wrongRuleTerm =
    "Incorrect rule for this term"


wrongRuleType =
    "Incorrect rule for this type"


topRuleParse =
    "Top rule parse Error"
