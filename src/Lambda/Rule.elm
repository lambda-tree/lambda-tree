module Lambda.Rule exposing (..)

import Lambda.Expression exposing (..)
import Maybe exposing (..)
import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)


type TyRule
    = TVar {
        -- bottom
       bottomCtx: Context,
        -- term
        bottomTerm: Term,
        --
        bottomTy: Ty,
        -- top
        -- ctx
        impliesCtx: Context,
        -- t
        impliesTerm: Term,
        -- ty
        impliesTy: Ty,
        -- the type ascription part -- ctx |- t: ty iff ...
        (Maybe TyRule)
} | TAbs {
    stm: TypeStatement,
    implStm: TypeStatement,
}

type alias TypeStatement = { ctx: Context, term: Term, ty: Ty }
type alias ContainmentStatement = { ctx: Context, variable: Term }



--
--  RuleIf (If t1.. t2.. t3..)
--  rule1 rule2 rule3
--
--  RuleIf term typeT
--      rule1 rule2 rule3
--
--
--
--  RuleIf (RuleIfBottom t ty) (RuleIfTop rule1 rule2 rule3)
--
--  RuleIf (RuleIfBottom (If (\\0) (\\1) (\\0)) ty) (RuleIfTop rule1 rule2 rule3)
--      if rule1.ty === Bool && rule2.ty === ty && rule3.ty === ty
--
--
--
--  Rule ruleType RuleBottom RuleTop
--  Rule RuleIf
--  RuleBottom = RuleBottom term type
--  RuleTop = [Rule]
--
--
--  checkRule ctx rule =
--      case rule of
--          (TermIf t1 t2 t3) ty


checkRule : TyRule -> Bool
checkRule rule =
    case rule of
        (TVar ctx1 term1 ty1 ctx2 term2 ty2 _) ->
            ctx1 == ctx2
             && term1 == term2
             && ty1 == ty2
             && (
                case term1 of
                    (TmVar _ x _) -> getbinding ctx2 x == Just (VarBind ty1)
                    _ -> False)
