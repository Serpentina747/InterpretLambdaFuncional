import Data.Map as Map

-- =================================
--       DEFINICIO DE DADES
-- =================================

type Var = String
type VarDB = Int
type Index = Map String Int
type Context = Map String Int

data LT = Variable Var | Abstr Var LT | Appli LT LT
data LTDB = VariableDB VarDB | AppliDB LTDB LTDB | AbstrDB LTDB deriving (Eq)



-- =================================
--            FUNCIONS
-- =================================


instance Show LT where
    show (Abstr var1 terme) = "(\\." ++ var1 ++ " " ++ show terme ++ ")"
    show (Appli terme_1 terme_2) = "(" ++ show terme_1 ++ " " ++ show terme_2 ++ ")"
    show (Variable var) = var

instance Eq LT where
    x == y = deBruijn x (freeVars x) == deBruijn y (freeVars y)

instance Ord LT where
    x <= y = boundVars x <= boundVars y

instance Show LTDB where
    show (AbstrDB terme) = "(\\." ++ show terme ++ ")"
    show (AppliDB terme_1 terme_2) = "(" ++ show terme_1 ++ " " ++ show terme_2 ++ ")"
    show (VariableDB var) = show var


--Paràmetres:   - Lambda Terme
--Funció:       - Retorna una llista de variables Var lliures
freeVars :: LT -> [Var]
freeVars (Variable x) = [x]
freeVars (Abstr x y) = Prelude.filter (/= x) (freeVars y)
freeVars (Appli x y) = freeVars x ++ freeVars y

--Paràmetres:   - Lambda Terme
--Funció:       - Retorna una llista de variables Var lligades
boundVars :: LT -> [Var]
boundVars (Variable x) = [x]
boundVars (Abstr x y) = Prelude.filter (== x) (freeVars y)
boundVars (Appli x y) = freeVars x ++ freeVars y

--Paràmetres:   - Variable Var
--              - llista de variables Var
--Funció:       - Retorna 'true' si la variable Var existeix a la llista, 'false' altrament
inlist :: Var -> [Var] -> Bool
inlist _ [] = False
inlist y (x:xs) = x == y || inlist y xs

--Paràmetres:   - llista de variables Var
--              - Lambda Terme
--              - Lambda Terme
--Funció:       - Retorna una variable Var que no estigui present com a variable lliure en els dos Lambda Termes
getAlfaValue :: [Var] -> LT -> LT -> Var
intlist [] _ _ = error "There's no alfa value for alfa conversion"
getAlfaValue  (x:xs) y z = if not(inlist x (freeVars y)) && not(inlist x (freeVars z)) then x else getAlfaValue xs y z


--Paràmetres:   - Lambda Terme
--              - Variable Var
--              - Lambda Terme
--Funció:       - Retorna un Lambda Terme al qual se li ha aplicat una substitució d'una variable per un altre Lambda Terme
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

--Paràmetres:   - Lambda Terme
--Funció:       - Retorna 'true' si el Lambda Terme correspon amb una abstracció, 'false' altrament
esAbstraccio :: LT -> Bool
esAbstraccio (Abstr x y) = True
esAbstraccio _ = False


--Paràmetres:   - Lambda Terme
--Funció:       - Retorna 'true' si el Lambda Terme està en forma normal, 'false' altrament.
estaNormal :: LT -> Bool
estaNormal (Variable x) = True
estaNormal (Appli term_a term_b)
 | esAbstraccio term_a = False
 | otherwise = estaNormal term_a && estaNormal term_b
estaNormal (Abstr abstr_value abstr_term) = estaNormal abstr_term

--Paràmetres:   - Lambda Terme
--Funció:       - En cas que el Lambda Terme sigui un redex, retorna el Lambda Terme havent resolt el redex, altrament retorna el mateix Lambda Terme.
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
lNormalitzaA lt = if not(estaNormal lt) then lt:lNormalitzaA (redueixUnA lt)
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


deBruijn :: LT -> [String] -> LTDB
deBruijn lt context = deBruijn2 lt empty (fromList (crearContext context 0))

deBruijn2 :: LT -> Index -> Context -> LTDB
deBruijn2 (Variable x) index context = if member x index then VariableDB (index ! x) else VariableDB (context ! x)
deBruijn2 (Abstr x y) index context = AbstrDB (deBruijn2 y (actualitza (insert x (-1) index )) (fromList (shiftContext  (toList context))))
deBruijn2 (Appli x y) index context = AppliDB (deBruijn2 x index context) (deBruijn2 y index context)

actualitza :: Index -> Index
actualitza index = fromList (crearIndex (toList index))

crearIndex ::[(String, Int)] -> [(String, Int)]
crearIndex [(x,y)] = [(x,y+1)]
crearIndex ((x,y):xs) = (x,y+1):crearIndex xs

crearContext :: [String] -> Int -> [(String, Int)]
crearContext [x] valor = [(x,valor)]
crearContext (x:xs) valor = (x,valor) : crearContext xs (valor + 1)

shiftContext ::  [(String, Int)] ->  [(String, Int)]
shiftContext [(x,y)] = [(x, y+1)]
shiftContext ((x,y):xs) = (x,y+1) : shiftContext xs





