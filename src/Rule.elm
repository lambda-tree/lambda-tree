module Rule exposing (..)

import Lambda exposing (..)
import Maybe exposing (..)

type TyRule
    = TVar
        ctx t ty -- the type ascription part -- ctx |- t: ty iff ...
        (Maybe TyRule)

    | TIf
        ctx ()



RuleIf (If t1.. t2.. t3..)
rule1 rule2 rule3

RuleIf term typeT
    rule1 rule2 rule3



RuleIf (RuleIfBottom t ty) (RuleIfTop rule1 rule2 rule3)

RuleIf (RuleIfBottom (If (\\0) (\\1) (\\0)) ty) (RuleIfTop rule1 rule2 rule3)
    if rule1.ty === Bool && rule2.ty === ty && rule3.ty === ty



Rule ruleType RuleBottom RuleTop
Rule RuleIf
RuleBottom = RuleBottom term type
RuleTop = [Rule]


checkRule ctx rule =
    case rule of
        (TermIf t1 t2 t3) ty

