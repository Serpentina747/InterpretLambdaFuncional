
-- =================================
--       DEFINICIO DE DADES
-- =================================

type Var = String
type VarDB = Int
type Index = (VarDB, Int)
type Context = [(String, Int)]

data LT = Variable Var | Abstr Var LT | Appli LT LT deriving (Eq)
data LTDB = VariableDB VarDB | AppliDB LTDB LTDB | AbstrDB LTDB deriving (Eq)



-- =================================
--            FUNCIONS
-- =================================

instance Show LT where
    show (Abstr var1 terme) = "(\\." ++ var1 ++ " " ++ show terme ++ ")"
    show (Appli terme_1 terme_2) = "(" ++ show terme_1 ++ " " ++ show terme_2 ++ ")"
    show (Variable var) = var

instance Show LTDB where
    show (AbstrDB terme) = "(\\." ++ show terme ++ ")"
    show (AppliDB terme_1 terme_2) = "(" ++ show terme_1 ++ " " ++ show terme_2 ++ ")"
    show (VariableDB var) = show var


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


betaRedueix :: LT -> LT
betaRedueix (Appli (Abstr a b) c) = subst b a c
betaRedueix a = a

redueixUnN :: LT -> LT
redueixUnN (Variable x) = Variable x
redueixUnN (Appli (Abstr x y) b) = betaRedueix (Appli (Abstr x y) b)
redueixUnN (Appli a b)
 | not (estaNormal a) = Appli (redueixUnN a) b
 | not (estaNormal b) = Appli a (redueixUnN b)
 | otherwise = Appli a b
redueixUnN (Abstr x y) = if estaNormal y then Abstr x y else Abstr x (redueixUnN y)


redueixUnA :: LT -> LT
redueixUnA (Variable x) = Variable x
redueixUnA (Appli a b)
 | not (estaNormal a) = Appli (redueixUnA a) b
 | not (estaNormal b) = Appli a (redueixUnA b)
 | not (estaNormal (Appli a b)) = betaRedueix (Appli a b) -- Redex
redueixUnA (Abstr x y) = if estaNormal y then Abstr x y else Abstr x (redueixUnA y)


lNormalitzaN :: LT -> [LT]
lNormalitzaN lt = if not(estaNormal lt) then lt:lNormalitzaN (redueixUnN lt)
                                else [lt]


lNormalitzaA :: LT -> [LT]
lNormalitzaA lt = if not(estaNormal lt) then lt:lNormalitzaA (betaRedueix lt)
                                else [lt]


normalitzaN :: LT -> (Int, LT)
normalitzaN lt = (steps, ltfinal)
    where
        steps = length(lNormalitzaN lt) - 1
        ltfinal = last(lNormalitzaN lt)


normalitzaA :: LT -> (Int, LT)
normalitzaA lt = (steps, ltfinal)
    where
        steps = length(lNormalitzaA lt) - 1
        ltfinal = last(lNormalitzaA lt)
