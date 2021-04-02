
type Var = String
data LT = Variable Var | Abstr Var LT | Appli LT LT deriving (Eq)

instance Show LT where
    show (Abstr var1 cos) = "(\\." ++ var1 ++ " " ++ show cos ++ ")"
    show (Appli terme_1 terme_2) = "(" ++ show terme_1 ++ " " ++ show terme_2 ++ ")"
    show (Variable var) = var

freeVars :: LT -> [Var]
freeVars (Variable x) = [x] 
freeVars (Abstr x y) = filter (/= x) (freeVars y)
freeVars (Appli x y) = freeVars x ++ freeVars y

boundVars :: LT -> [Var]
boundVars (Variable x) = [x]
boundVars (Abstr x y) = filter (== x) (freeVars y)
boundVars (Appli x y) = freeVars x ++ freeVars y

inlist :: Var -> [Var] -> Bool
inlist _ [] = False
inlist y (x:xs) = x == y || inlist y xs


getAlfaValue :: [Var] -> LT -> LT -> Var
intlist [] _ _ = error "There's no alfa value for alfa conversion"
getAlfaValue  (x:xs) y z = if not(inlist x (freeVars y)) && not(inlist x (freeVars z)) then x else getAlfaValue xs y z


subst :: LT -> Var -> LT -> LT
subst (Variable x) y z
 | x == y = z
 | otherwise = Variable x
subst (Appli term_a term_b) subst_value new_value = Appli (subst term_a subst_value new_value) (subst term_b subst_value new_value)
subst (Abstr abstr_value abstr_term) subst_value new_value
 | abstr_value == subst_value = Abstr abstr_value abstr_term
 | abstr_value /= subst_value && not(inlist abstr_value (freeVars new_value)) = Abstr abstr_value (subst abstr_term subst_value new_value)
 | abstr_value /= subst_value && inlist abstr_value (freeVars new_value) = Abstr (getAlfaValue alfabeticalList abstr_term new_value) 
                                        (subst (subst abstr_term abstr_value (Variable (getAlfaValue alfabeticalList abstr_term new_value))) subst_value new_value)
                                        where alfabeticalList = ["a", "b", "c", "d", "e"]

-- esta_normal :: LT -> Bool
-- esta_normal (Variable x) = True
-- esta_normal (Appli x y) = 
