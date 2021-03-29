
type Var = Char
data LT = Variable Var | Abstr Var LT | Appli LT LT deriving (Show, Eq)

freeVars :: LT -> [Var]
freeVars (Variable x) = [x]
freeVars (Abstr x y) = filter (/= x) (freeVars y)
freeVars (Appli x y) = freeVars x ++ freeVars y

boundVars :: LT -> [Var]
boundVars (Variable x) = [x]
boundVars (Abstr x y) = filter (== x) (freeVars y)
boundVars (Appli x y) = freeVars x ++ freeVars y

hies :: Var -> [Var] -> Bool
hies _ [] = False
hies y (x:xs) = x == y || hies y xs

subst :: LT -> Var -> LT -> LT
subst (Variable x) y z
 | x == y = z
 | otherwise = Variable x
subst (Appli term_a term_b) subst_value new_value = Appli (subst term_a subst_value new_value) (subst term_b subst_value new_value)
subst (Abstr abstr_value abstr_term) subst_value new_value
 | abstr_value == subst_value = Abstr abstr_value abstr_term
 | abstr_value /= subst_value && not(hies abstr_value (freeVars new_value)) = Abstr abstr_value (subst abstr_term subst_value new_value)
 -- | abstr_value /= subst_value && hies abstr_value (freeVars new_value) = Abstr (alfa_value abstr_term new_value) (subst abstr_term abstr_value (Variable alfa_value))
    -- where
        -- alfa_value  