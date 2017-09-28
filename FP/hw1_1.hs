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

