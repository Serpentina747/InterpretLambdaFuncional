{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Data.Map as Map

-- =================================
--       DEFINICIO DE DADES
-- =================================

type Var = String
type VarDB = Int
type Index = Map String Int
type Index2 = Map Int String
type Context = Map String Int

data LT = Variable Var | Abstr Var LT | Appli LT LT
data LTDB = VariableDB VarDB | AppliDB LTDB LTDB | AbstrDB LTDB


-- =================================
--    DEFINICIO DE METALLENGUATGE
-- =================================

identitat :: LT
identitat = (Abstr "x" (Variable "x"))

true :: LT
true =  (Abstr "x" (Abstr "y" (Variable "x")))

false :: LT
false = (Abstr "x" (Abstr "y" (Variable "y")))

-- Per no crear conflicte amb la operacio not, definim notM en ves de not
notM :: LT
notM = (Abstr "t" (Appli (Appli (Variable "t") (false)) (true)))

-- Per no crear conflicte amb la operacio and, definim andM en ves de and
andM :: LT
andM = (Abstr "x" (Abstr "y" (Appli (Appli (Variable "x") (Variable "y")) (Variable "x"))))

-- Per no crear conflicte amb la operacio or, definim orM en ves de or
orM :: LT
orM = (Abstr "x" (Abstr "y" (Appli (Appli (Variable "x") (Variable "x")) (Variable "y"))))

-- Per no crear conflicte amb la operacio xor, definim xorM en ves de xor
xorM :: LT
xorM = (Abstr "x" (Abstr "y" (Appli (Appli (orM) (Appli (Appli (andM) (Appli (notM) (Variable "x"))) (Variable "y"))) (Appli (Appli (andM) (Variable "x")) (Appli (notM) (Variable "y"))))))

suc :: LT
suc = (Abstr "n" (Abstr "f" (Abstr "x" (Appli (Variable "n") (Appli (Variable "f") (Appli (Variable "f") (Variable "x")))))))

fstM :: LT
fstM = (Abstr "x" (Appli (Variable "x") (true)))

sndM :: LT
sndM = (Abstr "x" (Appli (Variable "x") (false)))

suma :: LT
suma = (Abstr "m" (Abstr "n" (Abstr "f" (Abstr "x" (Appli (Appli (Variable "m") (Variable "f")) (Appli (Appli (Variable "n") (Variable "f")) (Variable "x")))))))

producte :: LT
producte = (Abstr "m" (Abstr "n" (Abstr "f" (Abstr "x" (Appli (Appli (Variable "m") (Appli (Variable "n") (Variable "f"))) (Variable "x"))))))

esZero :: LT
esZero = (Abstr "n" (Appli (Appli (Variable "n") (Abstr "x" false)) (true)))

tupla :: LT
tupla = (Abstr "x" (Abstr "y" (Abstr "p" (Appli (Appli (Variable "p") (Variable "x")) (Variable "y")))))

prefn :: LT
prefn = (Abstr "f" (Abstr "p" (Appli (Appli (tupla) (false)) (Appli (Appli (Appli (fstM) (Variable "p")) (Appli (sndM) (Variable "p"))) (Appli (Variable "f") (Appli (sndM) (Variable "p")))))))
--??f . ??p. [false , (fst p ??? snd p | f ( snd p))]

prec :: LT
prec = (Abstr "n" (Abstr "f" (Abstr "x" (Appli (sndM) (Appli (Appli (Variable "n") (Appli (prefn) (Variable "f"))) (Appli (Appli (tupla) (true)) (Variable "x")))))))
-- (??n. ??f . ??x. (snd (n (prefn f ) [true, x])))

prec2 :: LT
prec2 = (Abstr "n" (Abstr "f" (Abstr "x"  (Appli  (Appli (Appli (Variable "n") (Abstr "g" (Abstr "h" (Appli (Variable "h") (Appli (Variable "g") (Variable "f")))))) (Abstr "u" (Variable "x")))  (Abstr "u" (Variable "u"))))))

fact :: LT
fact = (Appli t (Abstr "f" (Abstr "n" (Appli (Appli (Appli esZero (Variable "n")) un) (Appli (Appli producte (Variable "n")) (Appli (Variable "f") (Appli prec2 (Variable "n"))))))))

-- =================================
--            COMBINADORS
-- =================================

k :: LT
k = (Abstr "x" (Abstr "y" (Variable "x")))

kPrima :: LT
kPrima = (Abstr "x" (Abstr "y" (Variable "y")))

s :: LT
s = (Abstr "x" (Abstr "y" (Abstr "z" (Appli (Appli (Variable "x") (Variable "z")) (Appli (Variable "y")(Variable "z"))))))

g :: LT
g = (Abstr "x" (Appli (Abstr "y" (Abstr "x" (Appli (Variable "y") (Variable "y")))) (Abstr "y" (Abstr "x" (Appli (Variable "y") (Variable "y"))))))
--??x.( (??y.(??x.y y)) (??y.(??x.y y)) )

y :: LT
y = (Abstr "f" (Appli (Abstr "x" (Appli (Variable "f") (Appli (Variable "x")(Variable "x")))) (Abstr "x" (Appli (Variable "f") (Appli (Variable "x") (Variable "x"))))))

t :: LT
t = (Appli (Abstr "x" (Abstr "y" (Appli (Variable "y") (Appli (Appli (Variable "x") (Variable "x")) (Variable "y"))))) (Abstr "x" (Abstr "y" (Appli (Variable "y") (Appli (Appli (Variable "x") (Variable "x")) (Variable "y"))))))


-- =================================
--            NATURALS
-- =================================

-- ENS SERVIRAN PER PROVAR LES FUNCIONS DEFINIDES A DALT

zero :: LT
zero = (Abstr "f" (Abstr "x" (Variable "x")))

un :: LT
un = (Abstr "f" (Abstr "x" (Appli (Variable "f") (Variable "x"))))

dos :: LT
dos = (Abstr "f" (Abstr "x" (Appli (Variable "f") (Appli (Variable "f") (Variable "x")))))

tres :: LT
tres = (Abstr "f" (Abstr "x" (Appli (Variable "f") (Appli(Variable "f") (Appli (Variable "f") (Variable "x"))))))

quatre :: LT
quatre = (Abstr "f" (Abstr "x" (Appli (Variable "f") (Appli (Variable "f") (Appli (Variable "f") (Appli (Variable "f") (Variable "x")))))))

-- =================================
--            FUNCIONS
-- =================================


instance Show LT where
    show (Abstr var1 terme) = "(\\" ++ var1 ++ ". " ++ show terme ++ ")"
    show (Appli terme_1 terme_2) = "(" ++ show terme_1 ++ " " ++ show terme_2 ++ ")"
    show (Variable var) = var

instance Eq LT where
    x == y = aDeBruijn x (freeVars x) == aDeBruijn y (freeVars y)

instance Ord LT where
    x <= y = boundVars x <= boundVars y

instance Show LTDB where
    show (AbstrDB terme) = "(\\." ++ show terme ++ ")"
    show (AppliDB terme_1 terme_2) = "(" ++ show terme_1 ++ " " ++ show terme_2 ++ ")"
    show (VariableDB var) = show var

instance Eq LTDB where
    VariableDB x == VariableDB y = x == y
    AppliDB x y == AppliDB z t = x == z && y == t
    AbstrDB x == AbstrDB y = x == y
    _ == _ = False

--Par??metres:   - Lambda Terme
--Funci??:       - Retorna una llista de variables Var lliures
freeVars :: LT -> [Var]
freeVars (Variable x) = [x]
freeVars (Abstr x y) = Prelude.filter (/= x) (freeVars y)
freeVars (Appli x y) = freeVars x ++ freeVars y

--Par??metres:   - Lambda Terme
--Funci??:       - Retorna una llista de variables Var lligades
boundVars :: LT -> [Var]
boundVars (Variable x) = [x]
boundVars (Abstr x y) = Prelude.filter (== x) (freeVars y)
boundVars (Appli x y) = freeVars x ++ freeVars y

--Par??metres:   - Variable a
--              - llista de variables a
--Funci??:       - Retorna 'true' si la variable a existeix a la llista, 'false' altrament
inlist :: Eq a => a -> [a] -> Bool
inlist y [] = False
inlist y [x] = y == x
inlist y (x:xs) = x == y || inlist y xs

--Par??metres:   - Una variable Var
--              - Lambda Terme
--              - Lambda Terme
--Funci??:       - Retorna una variable Var que no estigui present com a variable lliure en els dos Lambda Termes
getAlfaValue :: Var -> LT -> LT -> Var
getAlfaValue  x y z = if not(inlist x (freeVars y)) && not(inlist x (freeVars z))
    && not(inlist x (boundVars y)) && not(inlist x (boundVars y)) then x else getAlfaValue (x ++ "'") y z


--Par??metres:   - Lambda Terme
--              - Variable Var
--              - Lambda Terme
--Funci??:       - Retorna un Lambda Terme al qual se li ha aplicat una substituci?? d'una variable per un altre Lambda Terme
subst :: LT -> Var -> LT -> LT
subst (Variable x) y z
 | x == y = z
 | otherwise = Variable x
subst (Appli term_a term_b) subst_value new_value = Appli (subst term_a subst_value new_value) (subst term_b subst_value new_value)
subst (Abstr abstr_value abstr_term) subst_value new_value
 | abstr_value == subst_value = Abstr abstr_value abstr_term
 | abstr_value /= subst_value && not(inlist abstr_value (freeVars new_value)) = Abstr abstr_value (subst abstr_term subst_value new_value)
 | abstr_value /= subst_value && inlist abstr_value (freeVars new_value) = Abstr (getAlfaValue "x'" abstr_term new_value)
                                        (subst (subst abstr_term abstr_value (Variable (getAlfaValue "x'" abstr_term new_value))) subst_value new_value)

--Par??metres:   - Lambda Terme
--Funci??:       - Retorna 'true' si el Lambda Terme correspon amb una abstracci??, 'false' altrament
esAbstraccio :: LT -> Bool
esAbstraccio (Abstr x y) = True
esAbstraccio _ = False


--Par??metres:   - Lambda Terme
--Funci??:       - Retorna 'true' si el Lambda Terme est?? en forma normal, 'false' altrament
estaNormal :: LT -> Bool
estaNormal (Variable x) = True
estaNormal (Appli term_a term_b)
 | esAbstraccio term_a = False
 | otherwise = estaNormal term_a && estaNormal term_b
estaNormal (Abstr abstr_value abstr_term) = estaNormal abstr_term

--Par??metres:   - Lambda Terme
--Funci??:       - En cas que el Lambda Terme sigui un redex, retorna el Lambda Terme havent resolt el redex, altrament retorna el mateix Lambda Terme
betaRedueix :: LT -> LT
betaRedueix (Appli (Abstr a b) c) = subst b a c
betaRedueix a = a

--Par??metres:   - Lambda Terme
--Funci??:       - Retorna un LT que ??s el resultat de fer una beta-reducci?? en l'ordre normal
redueixUnN :: LT -> LT
redueixUnN (Variable x) = Variable x
redueixUnN (Appli (Abstr x y) b) = betaRedueix (Appli (Abstr x y) b)
redueixUnN (Appli a b)
 | not (estaNormal a) = Appli (redueixUnN a) b
 | not (estaNormal b) = Appli a (redueixUnN b)
 | otherwise = Appli a b
redueixUnN (Abstr x y) = if estaNormal y then Abstr x y else Abstr x (redueixUnN y)

--Par??metres:   - Lambda Terme
--Funci??:       - Retorna un LT que ??s el resultat de fer una beta-reducci?? en l'ordre associatiu
redueixUnA :: LT -> LT
redueixUnA (Variable x) = Variable x
redueixUnA (Appli a b)
 | not (estaNormal a) = Appli (redueixUnA a) b
 | not (estaNormal b) = Appli a (redueixUnA b)
 | not (estaNormal (Appli a b)) = betaRedueix (Appli a b) -- Redex
redueixUnA (Abstr x y) = if estaNormal y then Abstr x y else Abstr x (redueixUnA y)

--Par??metres:   - Lambda Terme
--Funci??:       - Retorna una llista de Lambda Termes amb la seq????ncia de reducci?? fins arribar a la forma normal (si en t??) seguint l'ordre de reducci?? normal
lNormalitzaN :: LT -> [LT]
lNormalitzaN lt = if not(estaNormal lt) then lt:lNormalitzaN (redueixUnN lt)
                                else [lt]


--Par??metres:   - Lambda Terme
--Funci??:       - Retorna una llista de Lambda Termes amb la seq????ncia de reducci?? fins arribar a la forma normal (si en t??) seguint l'ordre de reducci?? aplicatiu
lNormalitzaA :: LT -> [LT]
lNormalitzaA lt = if not(estaNormal lt) then lt:lNormalitzaA (redueixUnA lt)
                                else [lt]


--Par??metres:   - Lambda Terme
--Funci??:       - Retorna una tupla amb el nombre de passos de reducci??, seguint l'ordre de reducci?? normal, necessaris per arribar la forma normal
--              - (si en t??) i la seva forma normal.
normalitzaN :: LT -> (Int, LT)
normalitzaN lt = (steps, ltfinal)
    where
        steps = length(listLT) - 1
        ltfinal = last(listLT)
        listLT = lNormalitzaN lt

--Par??metres:   - Lambda Terme
--Funci??:       - Retorna una tupla amb el nombre de passos de reducci??, seguint l'ordre de reducci?? aplicatiu, necessaris per arribar la forma normal
--              - (si en t??) i la seva forma normal.
normalitzaA :: LT -> (Int, LT)
normalitzaA lt = (steps, ltfinal)
    where
        steps = length(listLT) - 1
        ltfinal = last(listLT)
        listLT = lNormalitzaA lt

--Par??metres:   - Lambda Terme
--              - Una llista de cadenes de car??cters que represent?? el context, el valor associat a cada cadena correspon
--              - inicialment amb la posici?? que ocupa en la llista
--Funci??:       - Retorna el Lambda Terme en la seva forma de Bruijn cridant a una funci?? que s'encarrega de fer la transformaci??
aDeBruijn :: LT -> [String] -> LTDB
aDeBruijn lt context = aDeBruijn2 lt empty (fromList (crearContext context 0))


--Par??metres:   - Lambda Terme
--              - Index
--              - Context
--Funci??:       - Retorna el Lambda Terme en la seva forma de Bruijn
aDeBruijn2 :: LT -> Index -> Context -> LTDB
aDeBruijn2 (Variable x) index context = if member x index then VariableDB (index ! x) else VariableDB (context ! x)
aDeBruijn2 (Abstr x y) index context = AbstrDB (aDeBruijn2 y (actualitza (insert x (-1) index )) (fromList (shift  (toList context))))
aDeBruijn2 (Appli x y) index context = AppliDB (aDeBruijn2 x index context) (aDeBruijn2 y index context)

--Par??metres:   - Index
--Funci??:       - Retorna un Index al qual se li han 'shiftat' els valors associats a les claus
actualitza :: Index -> Index
actualitza index = fromList (shift (toList index))

--Par??metres:   - Una llista de cadenes de car??cters
--              - Un enter
--Funci??:       - Retorna una llista de tuples on el primer valor correspon amb una cadena de car??cters i el segon un enter
--              - que correspon amb el valor inicial que t?? associada la cadena (valor que ve donat per la posici?? que ocupa
--              - en la llista passada com a par??metre)
crearContext :: [String] -> Int -> [(String, Int)]
crearContext [x] valor = [(x,valor)]
crearContext (x:xs) valor = (x,valor) : crearContext xs (valor + 1)

--Par??metres:   - Una llista de tuples que tenen com a primer valor una cadena de car??cters i com a segon un enter
--Funci??:       - Retorna una llista de tuples amb la mateixa forma que la llista entrada com a par??metre per?? amb
--              - els valors enters actualitzats una unitat m??s
shift ::  [(String, Int)] ->  [(String, Int)]
shift l = [(x,y+1) | (x,y) <- l]

--Par??metres:   - Una llista de tuples que tenen com a primer valor una cadena de car??cters i com a segon un enter
--Funci??:       - Retorna una llista de tuples amb la mateixa forma que la llista entrada com a par??metre per?? amb
--              - els valors enters actualitzats una unitat m??s
deDeBruijn :: LTDB -> (LT, Context)
deDeBruijn ltdb = deDeBruijn2 ltdb empty empty

--Par??metres:   - Un lambda terme en forma de Bruijn
--              - Un Index del segon tipus
--              - Un context
--Funci??:       - Retorna una tupla on el primer element ??s un lambda terme en format de LT i el segon correspon amb el context
--              - associat a les variables lliures
deDeBruijn2 :: LTDB -> Index2 -> Context -> (LT, Context)
deDeBruijn2 (VariableDB x) index context =  if member x index then (Variable (index ! x), context) else (Variable valorDif, insert valorDif x context)
    where
          valorDif = valorDiferent "x'" context (toList index)
deDeBruijn2 (AbstrDB x) index context = (Abstr novaVariable abstrLT, nouContext)
    where
          novaVariable = valorDiferent "x'" context (toList index)
          abstrLT = fst crida
          nouContext = snd crida
          crida = deDeBruijn2 x (fromList (actualitzarIndex novaVariable (toList index))) context
deDeBruijn2 (AppliDB x y) index context = (Appli esquerraLT dretaLT, contextDreta)
    where
        esquerraLT = fst cridaEsquerra
        dretaLT = fst cridaDreta
        contextEsquerra = snd cridaEsquerra
        cridaEsquerra = deDeBruijn2 x index context
        cridaDreta =  deDeBruijn2 y index contextEsquerra
        contextDreta =  snd cridaDreta

--Par??metres:   - Una cadena de car??cters
--              - Una llista de tuples on el primer element ??s un enter i el segon una cadena de car??cters
--Funci??:       - Retorna 'true' si la cadena de car??cters es troba com a segon element d'alguna de les tuples de la llista,
--              - retorna 'false' altrament
existeixVariable :: String -> [(Int, String)] -> Bool
existeixVariable var [] = False
existeixVariable var [(x,y)] = y == var
existeixVariable var ((x,y):xs) = y == var || existeixVariable var xs

--Par??metres:   - Una variable del tipus Var
--              - Una llista de tuples on el primer element ??s un enter i el segon una cadena de car??cters
--Funci??:       - Retorna una llista de tuples on el primer element ??s un enter i el segona una cadena de car??cters
--              - a la qual se li ha inserit la variable Var al final i s'han actualitzat una unitat les claus dels altres elements
actualitzarIndex :: Var -> [(Int, String)] -> [(Int, String)]
actualitzarIndex var [] = [(0, var)]            
actualitzarIndex var [(x,y)] = (0,var) : [(x+1,y)]    
actualitzarIndex var ((x,y): xs) = (x+1, y): actualitzarIndex var xs              

--Par??metres:   - Una cadena de car??cters
--              - Un Context
--              - Una llista de tuples on el primer element ??s un enter i el segon una cadena de car??cters 
--Funci??:       - Busca i retorna una variable del tipus Var que no es troba en el Context ni com a segon element
--              - de les tuples de la llista de tuples passada com a par??metre
valorDiferent :: String ->  Context -> [(Int, String)]  -> Var
valorDiferent var context l = if existeixVariable var l ||  member var context then valorDiferent (var ++ "'") context l else var