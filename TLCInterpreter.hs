import Data.Char (chr)

data Ty =
  TyBool
  | TyArr Ty Ty
  | TyPair Ty Ty
  | TyNat
  | TyAny
  | TyList Ty
  deriving (Show, Eq, Read)

data Term =
  -- Booleans
    TmTrue
  | TmFalse
  | TmZero
  -- Arithmetic
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  | TmPlus Term Term
  | TmMul Term Term
  | TmEq Term Term
  -- Conditionals
  | TmIf Term Term Term
  | TmVar Int
  | TmLam String Ty Term
  | TmApp Term Term
  -- Recursion
  | TmFix Term
  -- Logical operators
  | TmAnd Term Term
  | TmOr Term Term
  | TmNot Term
  -- Pair
  | TmPair Term Term
  | TmFst Term
  | TmSnd Term
  -- List
  | TmNil Ty
  | TmCons Term Term
  | TmIsNil Term
  | TmHead Term
  | TmTail Term
  deriving (Show, Eq, Read)

type Context = [(String, Ty)]


typeOf :: Context -> Term -> Either String Ty
typeOf ctx TmTrue  = Right TyBool
typeOf ctx TmFalse = Right TyBool
typeOf ctx TmZero  = Right TyNat

typeOf ctx (TmVar x) = getFromContext ctx x

typeOf ctx (TmLam name tyT1 t2) = do
  let ctx' = addToContext ctx name tyT1
  tyT2 <- typeOf ctx' t2
  Right (TyArr tyT1 tyT2)

typeOf ctx (TmApp t1 t2) = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  case tyT1 of
    TyArr tyT11 tyT12 ->
      if tyT2 == tyT11
        then Right tyT12
        else Left "Parameter type mismatch"
    _ -> Left "Arrow type expected"

typeOf ctx (TmIf t1 t2 t3) = do
  tyT1 <- typeOf ctx t1
  if tyT1 == TyBool
    then do
      tyT2 <- typeOf ctx t2
      tyT3 <- typeOf ctx t3
      if tyT2 == tyT3
        then Right tyT2
        else Left "Branches have different types"
    else Left "Guard of conditional is not a boolean"

typeOf ctx (TmSucc t1) = do
  tyT1 <- typeOf ctx t1
  if tyT1 == TyNat then Right TyNat else Left "Argument of succ is not a number"

typeOf ctx (TmPred t1) = do
  tyT1 <- typeOf ctx t1
  if tyT1 == TyNat then Right TyNat else Left "Argument of pred is not a number"

typeOf ctx (TmIsZero t1) = do
  tyT1 <- typeOf ctx t1
  if tyT1 == TyNat then Right TyBool else Left "Argument of iszero is not a number"

typeOf ctx (TmPlus t1 t2) = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  if tyT1 == TyNat && tyT2 == TyNat then Right TyNat else Left "Arguments of plus are not numbers"

typeOf ctx (TmEq t1 t2) = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  if (tyT1 == TyNat && tyT2 == TyNat) || (tyT1 == TyBool && tyT2 == TyBool)
    then Right TyBool
    else Left "Arguments of eq are not numbers or booleans"

typeOf ctx (TmAnd t1 t2) = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  if tyT1 == TyBool && tyT2 == TyBool then Right TyBool else Left "Arguments of and are not booleans"

typeOf ctx (TmOr t1 t2) = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  if tyT1 == TyBool && tyT2 == TyBool then Right TyBool else Left "Arguments of or are not booleans"

typeOf ctx (TmNot t1) = do
  tyT1 <- typeOf ctx t1
  if tyT1 == TyBool then Right TyBool else Left "Argument of not is not a boolean"

typeOf ctx (TmMul t1 t2) = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  if tyT1 == TyNat && tyT2 == TyNat then Right TyNat else Left "Arguments of mul are not numbers"

typeOf ctx (TmPair t1 t2) = do
  ty1 <- typeOf ctx t1
  ty2 <- typeOf ctx t2
  Right (TyPair ty1 ty2)

typeOf ctx (TmFst t) = do
  ty <- typeOf ctx t
  case ty of
    TyPair ty1 _ -> Right ty1
    _ -> Left "fst expects a pair"

typeOf ctx (TmSnd t) = do
  ty <- typeOf ctx t
  case ty of
    TyPair _ ty2 -> Right ty2
    _ -> Left "snd expects a pair"

typeOf ctx (TmNil ty) = Right (TyList ty)

typeOf ctx (TmCons h t) = do
  tyH <- typeOf ctx h
  tyT <- typeOf ctx t
  case tyT of
    TyList tyEl | tyEl == tyH -> Right (TyList tyEl)
    _ -> Left "Type mismatch in list cons"

typeOf ctx (TmIsNil t) = do
  tyT <- typeOf ctx t
  case tyT of
    TyList _ -> Right TyBool
    _ -> Left "isNil expects a list"

typeOf ctx (TmHead t) = do
  tyT <- typeOf ctx t
  case tyT of
    TyList tyEl -> Right tyEl
    _ -> Left "head expects a list"

typeOf ctx (TmTail t) = do
  tyT <- typeOf ctx t
  case tyT of
    TyList tyEl -> Right (TyList tyEl)
    _ -> Left "tail expects a list"

typeOf ctx (TmFix t1) = do
  ty1 <- typeOf ctx t1
  case ty1 of
    TyArr tyA tyB | tyA == tyB -> Right tyA
    _ -> Left "fix expects an argument of type T->T"


convertIndex :: Char -> Int -> [Char] -> Int
convertIndex c idx [] = idx
convertIndex c idx (y:ys)
  | c == y    = idx
  | otherwise = convertIndex c (idx + 1) ys

charFromIndex :: Int -> Char
charFromIndex n
  | n >= 0 && n <= 9 = chr (48 + n)
  | otherwise = error "Invalid index to convert"

rebuild :: String -> String -> String -> String
rebuild acc ctx [] = acc
rebuild acc ctx [x] = acc ++ [x]
rebuild acc ctx [x, y] = acc ++ [x, y]
rebuild acc ctx (x:y:z:xs)
  | x == '\\' = rebuild (acc ++ [x]) (ctx ++ [y]) (y:z:xs)
  | x `elem` " ([," && z `elem` " ,:)]" && y `elem` ctx =
      let position = convertIndex y 0 ctx
          symbol = charFromIndex (length ctx - position - 1)
      in rebuild (acc ++ [x, symbol]) ctx (z:xs)
  | x == ')' = rebuild (acc ++ [x]) (if null ctx then ctx else init ctx) (y:z:xs)
  | otherwise = rebuild (acc ++ [x]) ctx (y:z:xs)

addToContext :: Context -> String -> Ty -> Context
addToContext ctx x ty = (x, ty) : ctx

getFromContext :: Context -> Int -> Either String Ty
getFromContext ctx i
    | i < 0 || i >= length ctx = Left $ "Index " ++ show i ++ " out of bounds"
    | otherwise = Right (snd (ctx !! i))



isVal :: Term -> Bool
isVal t = case t of
    TmLam {} -> True
    TmTrue -> True
    TmFalse -> True
    TmZero -> True
    TmSucc t1 -> isNumericVal t1
    TmNil _ -> True
    TmCons h t -> isVal h && isVal t
    TmPair t1 t2 -> isVal t1 && isVal t2
    _ -> False

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _ = False



shift :: Int -> Term -> Term
shift d t = f 0 t
  where
    f c (TmVar x)        = if x >= c then TmVar (d + x) else TmVar x
    f c (TmLam n ty t1)  = TmLam n ty (f (c + 1) t1)
    f c (TmApp t1 t2)    = TmApp (f c t1) (f c t2)
    f c (TmIf t1 t2 t3)  = TmIf (f c t1) (f c t2) (f c t3)
    f c (TmSucc t1)      = TmSucc (f c t1)
    f c (TmPred t1)      = TmPred (f c t1)
    f c (TmIsZero t1)    = TmIsZero (f c t1)
    f c (TmPlus t1 t2)   = TmPlus (f c t1) (f c t2)
    f c (TmMul t1 t2)    = TmMul (f c t1) (f c t2)
    f c (TmEq t1 t2)     = TmEq (f c t1) (f c t2)
    f c (TmAnd t1 t2)    = TmAnd (f c t1) (f c t2)
    f c (TmOr t1 t2)     = TmOr (f c t1) (f c t2)
    f c (TmNot t1)       = TmNot (f c t1)
    f c (TmPair t1 t2)   = TmPair (f c t1) (f c t2)
    f c (TmFst t1)       = TmFst (f c t1)
    f c (TmSnd t1)       = TmSnd (f c t1)
    f c (TmCons h t1)    = TmCons (f c h) (f c t1)
    f c (TmIsNil t1)     = TmIsNil (f c t1)
    f c (TmHead t1)      = TmHead (f c t1)
    f c (TmTail t1)      = TmTail (f c t1)
    f c (TmFix t1)       = TmFix (f c t1)
    f _ TmTrue           = TmTrue
    f _ TmFalse          = TmFalse
    f _ TmZero           = TmZero
    f _ (TmNil ty)       = TmNil ty

subst :: Int -> Term -> Term -> Term
subst j s t = f 0 t
  where
    f c (TmVar x)        = if j + c == x then shift c s else TmVar x
    f c (TmLam n ty t1)  = TmLam n ty (f (c + 1) t1)
    f c (TmApp t1 t2)    = TmApp (f c t1) (f c t2)
    f c (TmIf t1 t2 t3)  = TmIf (f c t1) (f c t2) (f c t3)
    f c (TmSucc t1)      = TmSucc (f c t1)
    f c (TmPred t1)      = TmPred (f c t1)
    f c (TmIsZero t1)    = TmIsZero (f c t1)
    f c (TmPlus t1 t2)   = TmPlus (f c t1) (f c t2)
    f c (TmMul t1 t2)    = TmMul (f c t1) (f c t2)
    f c (TmEq t1 t2)     = TmEq (f c t1) (f c t2)
    f c (TmAnd t1 t2)    = TmAnd (f c t1) (f c t2)
    f c (TmOr t1 t2)     = TmOr (f c t1) (f c t2)
    f c (TmNot t1)       = TmNot (f c t1)
    f c (TmPair t1 t2)   = TmPair (f c t1) (f c t2)
    f c (TmFst t1)       = TmFst (f c t1)
    f c (TmSnd t1)       = TmSnd (f c t1)
    f c (TmCons h t1)    = TmCons (f c h) (f c t1)
    f c (TmIsNil t1)     = TmIsNil (f c t1)
    f c (TmHead t1)      = TmHead (f c t1)
    f c (TmTail t1)      = TmTail (f c t1)
    f c (TmFix t1)       = TmFix (f c t1)
    f _ TmTrue           = TmTrue
    f _ TmFalse          = TmFalse
    f _ TmZero           = TmZero
    f _ (TmNil ty)       = TmNil ty

beta :: Term -> Term -> Term
beta s t = shift (-1) $ subst 0 (shift 1 s) t


evalStep :: Term -> Either String Term

evalStep (TmApp (TmLam _ _ t12) v2) | isVal v2 = Right $ beta v2 t12
evalStep (TmApp t1 t2)
  | isVal t1 = case evalStep t2 of
                  Right t2' -> Right $ TmApp t1 t2'
                  Left err  -> Left err
  | otherwise = case evalStep t1 of
                  Right t1' -> Right $ TmApp t1' t2
                  Left err  -> Left err
evalStep t@(TmLam {}) = Right t

evalStep (TmIf TmTrue t2 _)  = Right t2
evalStep (TmIf TmFalse _ t3) = Right t3
evalStep (TmIf t1 t2 t3) = case evalStep t1 of
  Right t1' -> Right $ TmIf t1' t2 t3
  Left err  -> Left err
evalStep (TmSucc t1) = do
  t1' <- evalStep t1
  Right $ TmSucc t1'

evalStep (TmPred TmZero) = Right TmZero
evalStep (TmPred (TmSucc nv1)) | isNumericVal nv1 = Right nv1
evalStep (TmPred t1) = do
  t1' <- evalStep t1
  Right $ TmPred t1'

evalStep (TmIsZero TmZero) = Right TmTrue
evalStep (TmIsZero (TmSucc nv1)) | isNumericVal nv1 = Right TmFalse
evalStep (TmIsZero t1) = do
  t1' <- evalStep t1
  Right $ TmIsZero t1'

evalStep (TmPlus TmZero t) = Right t
evalStep (TmPlus (TmSucc t1) t2) = do
    t1' <- evalStep t1
    Right $ TmSucc (TmPlus t1' t2)
evalStep (TmPlus t1 t2)
    | isVal t1 = case evalStep t2 of
        Right t2' -> Right $ TmPlus t1 t2'
        Left err -> Left err
    | otherwise = case evalStep t1 of
        Right t1' -> Right $ TmPlus t1' t2
        Left err -> Left err

evalStep (TmMul TmZero _) = Right TmZero
evalStep (TmMul (TmSucc t1) t2) = do
    t1' <- evalStep t1
    t2' <- evalStep t2
    case evalStep (TmMul t1' t2') of
      Right t' -> Right $ TmPlus t2' t'
      Left err -> Left err
evalStep (TmMul t1 t2)
    | isVal t1 = case evalStep t2 of
        Right t2' -> Right $ TmMul t1 t2'
        Left err -> Left err
    | otherwise = case evalStep t1 of
        Right t1' -> Right $ TmMul t1' t2
        Left err -> Left err


evalStep (TmEq TmZero TmZero) = Right TmTrue
evalStep (TmEq (TmSucc _) TmZero) = Right TmFalse
evalStep (TmEq TmZero (TmSucc _)) = Right TmFalse
evalStep (TmEq (TmSucc t1) (TmSucc t2)) = Right $ TmEq t1 t2
evalStep (TmEq TmTrue TmTrue) = Right TmTrue
evalStep (TmEq TmFalse TmFalse) = Right TmTrue
evalStep (TmEq TmTrue TmFalse) = Right TmFalse
evalStep (TmEq TmFalse TmTrue) = Right TmFalse
evalStep (TmEq t1 t2)
    | not $ isNumericVal t1 = do
        t1' <- evalStep t1
        Right $ TmEq t1' t2
    | not $ isNumericVal t2 = do
        t2' <- evalStep t2
        Right $ TmEq t1 t2'
evalStep (TmEq _ _) = Right TmFalse



evalStep (TmAnd TmTrue t2) = Right t2
evalStep (TmAnd TmFalse _) = Right TmFalse
evalStep (TmAnd t1 t2) = do
    t1' <- evalStep t1
    Right $ TmAnd t1' t2

evalStep (TmOr TmTrue _) = Right TmTrue
evalStep (TmOr TmFalse t2) = Right t2
evalStep (TmOr t1 t2) = do
    t1' <- evalStep t1
    Right $ TmOr t1' t2

evalStep (TmNot TmTrue) = Right TmFalse
evalStep (TmNot TmFalse) = Right TmTrue
evalStep (TmNot t) = do
    t' <- evalStep t
    Right $ TmNot t'

evalStep (TmPair t1 t2)
  | isVal t1 && isVal t2 = Right (TmPair t1 t2)
  | not (isVal t1) = case evalStep t1 of
      Right t1' -> Right (TmPair t1' t2)
      Left err -> Left err
  | otherwise = case evalStep t2 of
      Right t2' -> Right (TmPair t1 t2')
      Left err -> Left err

evalStep (TmFst t) = case t of
  TmPair v1 _ | isVal v1 -> Right v1
  _ -> case evalStep t of
          Right t' -> Right (TmFst t')
          Left err -> Left err

evalStep (TmSnd t) = case t of
  TmPair _ v2 | isVal v2 -> Right v2
  _ -> case evalStep t of
          Right t' -> Right (TmSnd t')
          Left err -> Left err    

evalStep (TmIsNil (TmNil _)) = Right TmTrue
evalStep (TmIsNil (TmCons _ _)) = Right TmFalse
evalStep (TmIsNil t) = TmIsNil <$> evalStep t

evalStep (TmHead (TmCons h _)) = Right h
evalStep (TmHead t) = TmHead <$> evalStep t

evalStep (TmTail (TmCons _ t)) = Right t
evalStep (TmTail t) = TmTail <$> evalStep t

evalStep (TmCons h t)
  | isVal h && isVal t = Right (TmCons h t)
  | not (isVal h) = do h' <- evalStep h; Right $ TmCons h' t
  | otherwise = do t' <- evalStep t; Right $ TmCons h t'       

evalStep (TmFix (TmLam x ty t)) =
  Right $ beta (TmFix (TmLam x ty t)) t

evalStep (TmFix t1) =
  TmFix <$> evalStep t1

evalStep t
  | isVal t   = Right t
  | otherwise = Left "No rule applies"

eval :: Context -> Term -> Either String Term
eval ctx t = case typeOf ctx t of
    Left err -> Left $ "Type error: " ++ err
    Right _  -> case evalStep t of
        Right t' -> if t' == t then Right t else eval ctx t'
        Left err -> Left $ "Evaluation error: " ++ err

i2c :: Int -> Term
i2c 0 = TmZero
i2c n = TmSucc (i2c (n - 1))

c2i :: Term -> Int
c2i TmZero   = 0
c2i (TmSucc t) = 1 + c2i t
c2i _ = error "Not a numeric value"

s2t_var :: String
s2t_var = ['0'..'9']

s2t_lam :: String
s2t_lam = "\\"

s2t_sym :: String
s2t_sym = "()=,[]"

s2t_idt :: String
s2t_idt = ['a' .. 'z'] ++ ['0'..'9']

nonIdentifiers :: [String]
nonIdentifiers =
  ["true","false","zero","succ","plus","if","eq","and","or","not","iszero","mul","pair","fst","snd","nil","cons","head","tail","isnil","pred","fix"]     

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

splitArrow :: String -> Maybe (String, String)
splitArrow s =
  let go _ acc [] = Nothing
      go 0 acc ('-':'>':xs) = Just (reverse acc, xs)
      go n acc ('(' : xs)   = go (n+1) ('(' : acc) xs
      go n acc (')' : xs)   = go (n-1) (')' : acc) xs
      go n acc (x   : xs)   = go n (x:acc) xs
  in go 0 [] s

stringToType :: String -> Ty
stringToType raw =
  let s = trim raw
  in case splitArrow s of
       Just (l, r) -> TyArr (stringToType (trim l)) (stringToType (trim r))
       Nothing -> case s of
         "bool" -> TyBool
         "nat"  -> TyNat
         _      -> TyAny

typeToString :: Ty -> String
typeToString TyBool       = "bool"
typeToString TyNat        = "nat"
typeToString TyAny        = "any"
typeToString (TyArr a b)  =
  let left = case a of
               TyArr _ _ -> "(" ++ typeToString a ++ ")"
               _         -> typeToString a
  in left ++ "->" ++ typeToString b
typeToString (TyPair a b) = "(" ++ typeToString a ++ " * " ++ typeToString b ++ ")"
typeToString (TyList t)   = "list " ++ typeToString t

t2s :: Term -> String
t2s (TmVar x) = show x
t2s (TmLam var ty t) = s2t_lam ++ var ++ ": " ++ typeToString ty ++ "." ++ t2s t
t2s (TmApp s t@(TmApp _ _)) = t2s s ++ " (" ++ t2s t ++ ")"
t2s (TmApp s t@(TmLam {}))  = t2s s ++ " (" ++ t2s t ++ ")"
t2s (TmApp s@(TmLam {}) t)  = "(" ++ t2s s ++ ") " ++ t2s t
t2s (TmApp s t)             = t2s s ++ " " ++ t2s t
t2s s@(TmSucc t)            = "c" ++ show (c2i s)
t2s p@(TmPred t)            = "c" ++ show (c2i p)
t2s TmZero                  = "zero"
t2s TmTrue                  = "true"
t2s TmFalse                 = "false"
t2s (TmPlus s t)            = "plus " ++ t2s s ++ " " ++ t2s t
t2s (TmEq s t)              = "eq " ++ t2s s ++ " " ++ t2s t
t2s (TmCons h t)            = "[" ++ t2s h ++ listTail t ++ "]"
  where
    listTail (TmNil _)         = ""
    listTail (TmCons h' t')    = ", " ++ t2s h' ++ listTail t'
    listTail other             = " | " ++ t2s other
t2s (TmPair t1 t2)          = "(" ++ t2s t1 ++ ", " ++ t2s t2 ++ ")"
t2s (TmFst t)               = "fst " ++ t2s t
t2s (TmSnd t)               = "snd " ++ t2s t
t2s (TmFix t)               = "fix " ++ t2s t
t2s (TmMul s t)             = "mul " ++ t2s s ++ " " ++ t2s t
t2s (TmAnd s t)             = "and " ++ t2s s ++ " " ++ t2s t
t2s (TmOr  s t)             = "or "  ++ t2s s ++ " " ++ t2s t
t2s (TmNot t)               = "not " ++ t2s t
t2s (TmIsZero t)            = "iszero " ++ t2s t
t2s (TmHead t)              = "head " ++ t2s t
t2s (TmTail t)              = "tail " ++ t2s t
t2s (TmIsNil t)             = "isnil " ++ t2s t
t2s (TmNil _)               = "nil"

desugarListTokens :: [(String, String)] -> ([(String, String)], [(String, String)])
desugarListTokens (("[", "sym") : rest) = go rest []
  where
    go (("]", "sym") : tail) acc = (build acc, tail)
    go ((tok, typ) : ("," , "sym") : xs) acc = go xs (acc ++ [(tok, typ)])
    go ((tok, typ) : xs) acc = go xs (acc ++ [(tok, typ)])
    go [] _ = ([], [])
    build [] = [("nil", "nil")]
    build ts = foldr (\x acc -> [("cons", "cons"), x] ++ acc) [("nil", "nil")] ts
desugarListTokens s = ([], s)

s2t_static_mem :: [(String, Term)]
s2t_static_mem = [] ++ [('c' : show i, i2c i) | i <- [0 .. 1000]]

s2t_set_mem :: [(String, Term)] -> String -> Term -> [(String, Term)]
s2t_set_mem (p@(a, b) : m) s t =
  if a == s then (a, t) : m else p : s2t_set_mem m s t
s2t_set_mem [] s t = [(s, t)]

s2t_get_mem :: [(String, Term)] -> String -> [Term]
s2t_get_mem ((a, b) : m) s =
  if a == s then [b] else s2t_get_mem m s
s2t_get_mem [] _ = []

s2t_read_var :: String -> (String, String)
s2t_read_var s = f ("", s)
  where f (t, s@(c : cs)) = if c `elem` s2t_var then f (c : t, cs) else (reverse t, s)
        f (t, "") = (reverse t, "")

s2t_read_lam :: String -> (String, String)
s2t_read_lam s
  | h == s2t_lam = let (a, b) = span (/= '.') (drop l s)
                   in (h ++ a ++ ".", drop 1 b)
  | otherwise = ("", s)
  where h = take l s
        l = length s2t_lam

extractVarAndType :: String -> (String, String)
extractVarAndType s = (a, dropWhile (== ' ') $ drop 1 b)
  where (a, b) = span (/= ':') c
        c = init (drop 1 s)

s2t_read_idt :: String -> (String, String)
s2t_read_idt s = f ("", s)
  where f (t, s@(c : cs)) = if c `elem` s2t_idt then f (c : t, cs) else (reverse t, s)
        f (t, "") = (reverse t, "")

s2t_tokenize :: String -> [(String, String)] -> [(String, String)]
s2t_tokenize s@(c : cs) t
  | c == ' ' = s2t_tokenize cs t
  | c `elem` s2t_var = if v == [] then [] else s2t_tokenize vs ((v, "var") : t)
  | c == head s2t_lam = if l == [] then [] else s2t_tokenize ls ((l, "lam") : t)
  | c `elem` s2t_sym = s2t_tokenize cs (([c], "sym") : t)
  | c `elem` s2t_idt =
      let (i, is) = s2t_read_idt s
      in if i `elem` nonIdentifiers
         then s2t_tokenize is ((i, i) : t)
         else s2t_tokenize is ((i, "idt") : t)
  | otherwise = []
  where (v, vs) = s2t_read_var s
        (l, ls) = s2t_read_lam s
s2t_tokenize "" t = reverse t


s2t_interpret_var :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_var m ((n, "var") : s) =
  if all (`elem` s2t_var) n then ([TmVar (read n :: Int)], s) else ([], [])

s2t_interpret_lst :: [(String, Term)] -> [(String, String)] -> [Term] -> ([Term], [(String, String)])
s2t_interpret_lst m s@((")", "sym") : _) l = (reverse l, s)
s2t_interpret_lst m [] l = (reverse l, [])
s2t_interpret_lst m s l = if t' == [] then ([], []) else s2t_interpret_lst m s' (t' ++ l)
  where (t', s') = s2t_interpret_term m s

s2t_interpret_grp :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_grp m (("(", "sym") : s)
  | l == [] || s' == [] = ([], [])
  | head s' == (")", "sym") = ([foldl (\a e -> TmApp a e) (head l) (tail l)], tail s')
  | otherwise = ([], [])
  where (l, s') = s2t_interpret_lst m s []

s2t_interpret_lam :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_lam m ((lamDef, "lam") : s)
  | l /= [] = ([TmLam varName varType (foldl (\a e -> TmApp a e) (head l) (tail l))], s')
  | otherwise = ([], [])
  where (l, s') = s2t_interpret_lst m s []
        (varName, typeName) = extractVarAndType lamDef
        varType = stringToType typeName

s2t_interpret_idt :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_idt m ((i, "idt") : s) = if t == [] then ([], []) else (t, s)
  where t = s2t_get_mem m i


s2t_interpret_true m (("true", "true") : s) = ([TmTrue], s)
s2t_interpret_false m (("false", "false") : s) = ([TmFalse], s)
s2t_interpret_zero m (("zero", "zero") : s) = ([TmZero], s)

s2t_interpret_succ m (("succ", "succ") : s) =
  case s2t_interpret_term m s of
    ([t], s') -> ([TmSucc t], s')
    _ -> ([], [])

s2t_interpret_pred m (("pred", "pred") : s) =
  case s2t_interpret_term m s of
    ([t], s') -> ([TmPred t], s')
    _ -> ([], [])

s2t_interpret_plus m (("plus", "plus") : s) =
  case s2t_interpret_term m s of
    ([t1], s1) ->
      case s2t_interpret_term m s1 of
        ([t2], s2) -> ([TmPlus t1 t2], s2)
        _ -> ([], [])
    _ -> ([], [])

s2t_interpret_mul m (("mul", "mul") : s) =
  case s2t_interpret_term m s of
    ([t1], s1) ->
      case s2t_interpret_term m s1 of
        ([t2], s2) -> ([TmMul t1 t2], s2)
        _ -> ([], [])
    _ -> ([], [])

s2t_interpret_eq m (("eq", "eq") : s) =
  case s2t_interpret_term m s of
    ([t1], s1) ->
      case s2t_interpret_term m s1 of
        ([t2], s2) -> ([TmEq t1 t2], s2)
        _ -> ([], [])
    _ -> ([], [])

s2t_interpret_if m (("if", "if") : s) =
  case s2t_interpret_term m s of
    ([t1], s1) ->
      case s2t_interpret_term m s1 of
        ([t2], s2) ->
          case s2t_interpret_term m s2 of
            ([t3], s3) -> ([TmIf t1 t2 t3], s3)
            _ -> ([], [])
        _ -> ([], [])
    _ -> ([], [])

s2t_interpret_and m (("and", "and") : s) =
  case s2t_interpret_term m s of
    ([t1], s1) ->
      case s2t_interpret_term m s1 of
        ([t2], s2) -> ([TmAnd t1 t2], s2)
        _ -> ([], [])
    _ -> ([], [])

s2t_interpret_or m (("or", "or") : s) =
  case s2t_interpret_term m s of
    ([t1], s1) ->
      case s2t_interpret_term m s1 of
        ([t2], s2) -> ([TmOr t1 t2], s2)
        _ -> ([], [])
    _ -> ([], [])

s2t_interpret_not m (("not", "not") : s) =
  case s2t_interpret_term m s of
    ([t], s') -> ([TmNot t], s')
    _ -> ([], [])

s2t_interpret_iszero m (("iszero", "iszero") : s) =
  case s2t_interpret_term m s of
    ([t], s') -> ([TmIsZero t], s')
    _ -> ([], [])

s2t_interpret_pair m (("pair", _) : s) =
  case s2t_interpret_term m s of
    ([t1], s1) -> case s2t_interpret_term m s1 of
      ([t2], s2) -> ([TmPair t1 t2], s2)
      _ -> ([], [])
    _ -> ([], [])

s2t_interpret_fst m (("fst", _) : s) =
  case s2t_interpret_term m s of
    ([t], s') -> ([TmFst t], s')
    _ -> ([], [])

s2t_interpret_snd m (("snd", _) : s) =
  case s2t_interpret_term m s of
    ([t], s') -> ([TmSnd t], s')
    _ -> ([], [])

s2t_interpret_nil m (("nil", _) : s) = ([TmNil TyAny], s)

s2t_interpret_cons m (("cons", _) : s) =
  case s2t_interpret_term m s of
    ([h], s1) ->
      case s2t_interpret_term m s1 of
        ([TmNil _], s2) ->
          case typeOf [] h of
            Right ty -> ([TmCons h (TmNil ty)], s2)
            _ -> ([TmCons h (TmNil TyAny)], s2)
        ([t], s2) -> ([TmCons h t], s2)
        _ -> ([], [])
    _ -> ([], [])

s2t_interpret_head m (("head", _) : s) =
  case s2t_interpret_term m s of
    ([t], s') -> ([TmHead t], s')
    _ -> ([], [])

s2t_interpret_tail m (("tail", _) : s) =
  case s2t_interpret_term m s of
    ([t], s') -> ([TmTail t], s')
    _ -> ([], [])

s2t_interpret_isnil :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_isnil m (("isnil", _) : s) =
  case s2t_interpret_term m s of
    ([t], s') -> ([TmIsNil t], s')
    _ -> ([], [])


s2t_interpret_fix :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_fix m (("fix","fix") : s) =
  case s2t_interpret_term m s of
    ([t], s') -> ([TmFix t], s')
    _         -> ([], [])

s2t_interpret_term :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_term m s@(("[" , "sym") : _) =
  let (desugared, _) = desugarListTokens s
      wrapped = ("(", "sym") : desugared ++ [(")", "sym")]
  in s2t_interpret_term m wrapped

s2t_interpret_term m s@((a, b) : _)
  | a == "(" && b == "sym" = s2t_interpret_grp m s
  | b == "var"     = s2t_interpret_var     m s
  | b == "lam"     = s2t_interpret_lam     m s
  | b == "idt"     = s2t_interpret_idt     m s
  | b == "true"    = s2t_interpret_true    m s
  | b == "false"   = s2t_interpret_false   m s
  | b == "zero"    = s2t_interpret_zero    m s
  | b == "succ"    = s2t_interpret_succ    m s
  | b == "pred"    = s2t_interpret_pred    m s
  | b == "plus"    = s2t_interpret_plus    m s
  | b == "mul"     = s2t_interpret_mul     m s
  | b == "eq"      = s2t_interpret_eq      m s
  | b == "if"      = s2t_interpret_if      m s
  | b == "and"     = s2t_interpret_and     m s
  | b == "or"      = s2t_interpret_or      m s
  | b == "not"     = s2t_interpret_not     m s
  | b == "iszero"  = s2t_interpret_iszero  m s
  | b == "pair"    = s2t_interpret_pair    m s
  | b == "fst"     = s2t_interpret_fst     m s
  | b == "snd"     = s2t_interpret_snd     m s
  | a == "fix"     = s2t_interpret_fix     m s
  | a == "nil"     = s2t_interpret_nil     m s
  | a == "cons"    = s2t_interpret_cons    m s
  | a == "head"    = s2t_interpret_head    m s
  | a == "tail"    = s2t_interpret_tail    m s
  | a == "isnil"   = s2t_interpret_isnil   m s
  | otherwise      = ([], [])

s2t_interpret_assign :: [(String, Term)] -> [(String, String)] -> [(String, Term)]
s2t_interpret_assign m ((a, "idt") : ("=", "sym") : t) =
  if l == [] || s /= [] then [] else s2t_set_mem m a (head l)
  where
    t' = case t of
      ("[" , "sym") : _ ->
        let (desugared, _) = desugarListTokens t
        in ("(", "sym") : desugared ++ [(")", "sym")]
      _ ->
        ("(", "sym") : t ++ [(")", "sym")]

    (l, s) = s2t_interpret_term m t'


s2t_interpreter :: [(String, Term)] -> (Term -> String) -> IO ()
s2t_interpreter m f =
  do putStr ">> "
     l <- getLine
     if l == [] then do
       s2t_interpreter m f
     else if l == "-h" || l == "-help" then do
       putStr "Commands:\n"
       putStr "-lambda: lambda notation\n"
       putStr "-int: integer notation\n"
       putStr "-raw: raw output\n"
       putStr "-exit: shut down the interpreter\n"
       putStr "-clear: clear screen\n"
       s2t_interpreter m f
     else if l == "-lambda" then do
       putStr "Setting lambda notation.\n"
       s2t_interpreter m t2s
     else if l == "-int" then do
       putStr "Setting integer notation.\n"
       s2t_interpreter m (\x -> let n = c2i x in if n == -1 then "NaN" else 'c' : show n)
     else if l == "-raw" then do
       putStr "Switching to raw output.\n"
       s2t_interpreter m (\x -> show x)
     else if l == "-exit" then do
       putStr "The end.\n"
       return ()
     else if l == "-clear" then do
       putStr $ replicate 72 '\n'
       s2t_interpreter m f
     else if head l == '-' then do
       putStr $ "Unrecognized command.\n"
       s2t_interpreter m f
     else do
       let s = s2t_tokenize (rebuild [] [] (' ' : l)) []
       if s == [] then do
         putStr "Invalid input.\n"
         s2t_interpreter m f
       else if ("=", "sym") `elem` s then do
         let n = s2t_interpret_assign m s
         if n == [] then do
           putStr "Invalid input.\n"
           s2t_interpreter m f
         else do
           putStr $ (fst . head) s ++ " defined.\n"
           s2t_interpreter n f
       else do
         let (t, r) = s2t_interpret_term m (("(", "sym") : s ++ [(")", "sym")])
         if t == [] || r /= [] then do
           putStr "Invalid input.\n"
         else do
            let evalResult = eval [] (head t)
            case evalResult of
                Left err -> putStrLn err
                Right term -> do
                    let p = f term
                    if p /= "" then putStrLn p
                    else do
                        let termString = t2s term
                        putStrLn termString
         s2t_interpreter m f

main :: IO ()
main = do
  putStr "Haskell interpreter of the simply typed lambda calculus.\n"
  putStr "Input -h for help, exit to terminate the program.\n"
  s2t_interpreter s2t_static_mem t2s
