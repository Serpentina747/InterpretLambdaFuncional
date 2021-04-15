
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

esAbstraccio :: LT -> Bool
esAbstraccio (Abstr x y) = True
esAbstraccio _ = False



estaNormal :: LT -> Bool
estaNormal (Variable x) = True
estaNormal (Appli term_a term_b)
 | esAbstraccio term_a = False 
 | otherwise = estaNormal term_a && estaNormal term_b
estaNormal (Abstr abstr_value abstr_term) = estaNormal abstr_term


beta_redueix :: LT -> LT
beta_redueix (Appli (Abstr a b) c) = subst b a c
beta_reudeix a = a

--redueix_un_n :: LT -> LT
--redueix_un_n (Variable x) = Variable x
--redueix_un_n (Abstr x y)
-- | not (estaNormal y) = beta_redueix y
-- | otherwise = Abstr x y
--redueix_un_n (Appli x y)
-- | not (estaNormal x) = beta_redueix x
-- | not (estaNormal y) = beta_redueix y
-- | otherwise = Appli x y


redueix_un_n :: LT -> LT
redueix_un_n (Variable x) = Variable x
redueix_un_n (Appli (Abstr x y) b) = beta_redueix (Appli (Abstr x y) b)
redueix_un_n (Appli a b)
 | not (estaNormal a) = (Appli (redueix_un_n a) b)
 | not (estaNormal b) = (Appli a (redueix_un_n b))
 | otherwise = Appli a b
redueix_un_n (Abstr x y) = if estaNormal y then Abstr x y else Abstr x (redueix_un_n y)


redueix_un_a :: LT -> LT
redueix_un_a (Variable x) = Variable x
redueix_un_a (Appli a b)
 | not (estaNormal a) = (Appli (redueix_un_a a) b)
 | not (estaNormal b) = (Appli a (redueix_un_a b))
 | not (estaNormal (Appli a b)) = beta_redueix (Appli a b) -- Redex
redueix_un_a (Abstr x y) = if estaNormal y then Abstr x y else Abstr x (redueix_un_a y)