import Control.Lens hiding (op)
import Term
import ReverseList

getLeft :: Term -> Term
getLeft (BinaryTerm l r op) = getLeft l
getLeft (UnaryTerm op t)= getLeft t
getLeft t = t

setLeft :: Term -> Term -> Term
setLeft (BinaryTerm l r op) x = BinaryTerm (setLeft l x) r op
setLeft (UnaryTerm op t) x = UnaryTerm op (setLeft t x) 
setLeft t x = x

leftTermLens :: Lens' Term Term 
leftTermLens = lens getLeft setLeft

getFirstMult :: Term -> Maybe (Term, Term)
getFirstMult (UnaryTerm _ t) = getFirstMult t
getFirstMult (BinaryTerm l r op) | op == Mult = Just (l,r)
                                 | otherwise = if leftRes == Nothing then getFirstMult r
                                               else leftRes
                                    where leftRes = getFirstMult l
getFirstMult _ = Nothing
 
setFirstMult :: Term -> Maybe (Term, Term) -> Term
setFirstMult t Nothing = t
setFirstMult (UnaryTerm _ t) p = setFirstMult t p
setFirstMult (BinaryTerm l r op) p@(Just (l0,r0)) | op == Mult = BinaryTerm l0 r0 op
                                                  | otherwise = if getFirstMult l == Nothing then BinaryTerm l (setFirstMult r p) op
                                               else BinaryTerm (setFirstMult l p) r op
setFirstMult t _ = t

firstMultLens :: Lens' Term (Maybe (Term, Term))
firstMultLens = lens getFirstMult setFirstMult

--tests--
veryBigTerm = BinaryTerm (BinaryTerm (UnaryTerm Minus (IntConstant 4)) (BinaryTerm (Variable "a") (IntConstant 2) Mult) Plus) (BinaryTerm (BinaryTerm (IntConstant 42) (IntConstant 4) Mult) (BinaryTerm (Variable "a") (IntConstant 2) Mult) Plus) Plus

leftTestGet = veryBigTerm ^. leftTermLens
leftTestSet = veryBigTerm & leftTermLens .~ (Variable "replaced term")

multTestGet = veryBigTerm ^. firstMultLens
multTestSet = veryBigTerm & firstMultLens .~ Just (Variable "replaced", Variable "terms")


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
