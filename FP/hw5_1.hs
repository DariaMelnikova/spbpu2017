import Control.Lens hiding (op)
import Term
import ReverseList

valueLens :: Lens' Term Term
valueLens = lens value (\ term newValue -> term { value = newValue })

variableLens :: Lens' Term String
variableLens = lens varName (\ term newVarName -> term { varName = newVarName })

operatorLens :: Lens' Term Operator
operatorLens = lens op (\ term newOp -> term { op = newOp })

-- tests --
var = Variable "var"
uno = UnaryTerm Minus var
bin = BinaryTerm var uno Plus

valueTestGet = uno ^. valueLens
valueTestSet = uno & valueLens .~ (IntConstant 0)

variableTestGet = var ^. variableLens
variableTestSet = var & variableLens .~ "something else"

operatorTestGet = bin ^. operatorLens
operatorTestSet = bin & operatorLens .~ Minus


headLens :: Lens' (ReverseList a) a
headLens = let get (RCons RNil a) = a
               get RNil = error "Empty list"
               get (RCons x a) = get x in
           let set (RCons RNil a) b = RCons RNil b
               set RNil _ = error "Empty list"
               set (RCons x a) b = set x b in
           lens get set

tailLens :: Lens' (ReverseList a) a
tailLens = let get (RCons _ a) = a
               get _ = error "Empty list" in
           let set (RCons x a) b = RCons x b
               set _ _ = error "Empty list" in
           lens get set


-- tests --
revList = toRList [0, 1, 2, 3, 4]

headTestGet = revList ^. headLens
headTestSet = revList & headLens .~ 8

tailTestGet = revList ^. tailLens
tailTestSet = revList & tailLens .~ 0
