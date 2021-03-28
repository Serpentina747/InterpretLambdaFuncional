
type Var = Char
data LT = Variable Var | Abstr Var LT | Appli LT LT deriving (Eq, Show)

freeVars :: LT -> [Var]
freeVars (Variable x) = [x]
freeVars (Abstr x y) = filter (/= x) (freeVars y)
freeVars (Appli x y) = freeVars x ++ freeVars y

boundVars :: LT -> [Var]
boundVars (Variable x) = [x]
boundVars (Abstr x y) = filter (== x) (freeVars y)
boundVars (Appli x y) = freeVars x ++ freeVars y