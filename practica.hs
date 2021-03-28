
type Var = Char
data LT = Variable Var | Abstr Var LT | Appli LT LT deriving (Eq, Show)

freeVars :: LT -> [Var]
freeVars (Variable x) = [x]
