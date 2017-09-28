data Operator = Plus | Minus | Mult deriving (Show, Eq)

data Term = IntConstant { intValue::Int }
          | Variable { varName::String }
          | BinaryTerm { lhv::Term, rhv::Term, op::Operator } 
          | UnaryTerm {op::Operator, value::Term} deriving (Show, Eq)

infixl 6 <+>
(IntConstant a) <+> (IntConstant b) = IntConstant (a + b)
a <+> b = BinaryTerm a b Plus

infixl 6 <->
(IntConstant a) <-> (IntConstant b) = IntConstant (a - b)
a <-> b = BinaryTerm a b Minus

infixl 7 <*>
(IntConstant a) <*> (IntConstant b) = IntConstant (a * b)
a <*> b = BinaryTerm a b Mult

replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant c) _ _ = IntConstant c
replaceVar (Variable v) name term = if v == name then term else Variable v
replaceVar (BinaryTerm l r op) name term = BinaryTerm (replaceVar l name term) (replaceVar r name term) op
replaceVar (UnaryTerm op v) name term = UnaryTerm op (replaceVar v name term)
