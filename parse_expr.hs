import Data.Char
import Data.List


data Op = Plus | Mult
    deriving (Show, Eq)

data ArithExpr = Number Integer | Var String | Operator Op ArithExpr ArithExpr
    deriving (Show, Eq)


     
--evaluating an expression, given a list of variables    
eval :: ArithExpr -> [(String, Integer)] -> Integer
eval (Number n) varlist = n
eval (Operator Plus e1 e2) varlist = (eval e1 varlist) + (eval e2 varlist) -- Evaluate all subexpressions then add
eval (Operator Mult e1 e2) varlist = (eval e1 varlist) * (eval e2 varlist)
eval (Var x) varlist = (\(Just a) -> a) $ lookup x varlist



parseExpr :: String -> ArithExpr
-- first we break up into tokens, then convert those into an expression
parseExpr = buildExpr . readTokens


readToken :: String -> (String, String) -- First string is token
readToken (x:xs)
    -- Ignore spaces
    | isSpace x  = readToken xs
    -- Token is negative number
    | x == '-' && isDigit (head xs) = (x:(takeWhile isDigit xs),
                                            dropWhile isDigit xs) 
    -- Token is positive number
    | isDigit x = (takeWhile isDigit (x:xs),
                             dropWhile isDigit xs)
    | isLetter x = (takeWhile isLetter (x:xs),
                             dropWhile isLetter xs)
    -- Token is operator
    | x `elem` ['+','*'] = ([x], xs) -- Token is an operator
    | x == '(' = readBracketed (x:xs) -- Token is bracketed subexpression
    | otherwise = error ("Unrecognised token at head of " ++ (x:xs))

readTokens :: String -> [String]
readTokens xs = if xs == ""  -- Base case
                    then [] -- no more tokens
                    else token:(readTokens rest)
                    -- Otherwise, read a single token
                    -- from the start of the string and recurse
    where (token, rest) = readToken xs

-- buildExpr used for both simple and complex building
buildExpr :: [String] -> ArithExpr
buildExpr ts
    | (length ts) == 1 && isInt (head ts) =  Number (read (head ts))
    | (length ts) == 1 && all isLetter (head ts) =  Var (head ts)
    -- If it is a single token and not an int or letter, the only valid possibility
    -- is that it is a subexpression
    | (length ts) == 1 = parseExpr (head ts)
    -- Plus is checked first because it has lower precedence
    | "+" `elem` ts = Operator Plus (buildExpr plus1) (buildExpr $ tail plus2)
    | "*" `elem` ts = Operator Mult (buildExpr times1) (buildExpr $ tail times2)
    | otherwise = error ("invalid tokens " ++ (show ts))
    where (times1, times2) = break (=="*") ts    
          (plus1, plus2) = break (=="+") ts
    
    
isInt :: String -> Bool
isInt (x:xs) = all isDigit xs && ((x == '-') || isDigit x)

-- the first character in the string should be a left
-- parenthesis.  This function reads up until the matching
-- right parenthesis, and returns that string and the remainder
readBracketed :: String -> (String, String)
readBracketed ('(':xs) = findMatching 0 "" xs
    -- the first argument in findMatching keeps track of
    -- how many extra layers of parentheses we are inside
    where -- we found the matching parenthesis as the count is down to zero
          findMatching 0 matched (')':xs) = (matched, xs) 
          -- entering another layer of parentheses
          findMatching n matched ('(':xs) = findMatching (n + 1) (matched ++ "(") xs
          -- exited a layer of parentheses
          findMatching n matched (')':xs) = findMatching (n - 1) (matched ++ ")") xs
          -- non-parenthesis character
          findMatching n matched (x:xs) = findMatching n (matched ++ [x]) xs

--I included my functions for differentiation and simplification here, but my modifications to allow variables are in the same file as the sample solution.

--differentiation
d :: String -> ArithExpr -> ArithExpr
d x (Number n) = Number 0
d x (Var y)
	| y == x = Number 1
	| otherwise = Number 0
d x (Operator Mult e1 e2) = Operator Plus (Operator Mult (d x e1) e2) (Operator Mult (d x e2) e1) 
d x (Operator Plus e1 e2) = Operator Plus (d x e1) (d x e2)


--Here is where I combine all my simplifying functions.  I run the main simplifying function before and after simplifying multiplication and division, because sometimes it can be simplified 
--further after it is reshuffled with simplifyAddition and simplifyMultiplication
simplify = baseSimplify . simplifyMultiplication . simplifyAddition . baseSimplify


--basic simplification rules
baseSimplify :: ArithExpr -> ArithExpr
baseSimplify (Number x) = Number x --base cases
baseSimplify (Var x) = Var x

baseSimplify (Operator Plus (Number 0) x) = baseSimplify x --basic addition/multiplication simplification (additive/multiplicative identities)
baseSimplify (Operator Plus x (Number 0)) = baseSimplify x
baseSimplify (Operator Mult (Number 1) x) = baseSimplify x
baseSimplify (Operator Mult x (Number 1)) = baseSimplify x

baseSimplify (Operator Mult (Number (-1)) (Number x)) = Number (-x) --inverses
baseSimplify (Operator Mult (Number x) (Number (-1))) = Number (-x)

baseSimplify (Operator Mult (Number 0) x) = Number 0 --zero property of multiplication
baseSimplify (Operator Mult x (Number 0)) = Number 0

baseSimplify p@(Operator op (Number a) (Number b)) = Number $ eval p [] --if expr just contains numbers, baseSimplify to a number

baseSimplify e@(Operator op p q@(Operator subop2 subex21 subex22)) --only want to keep simplifying if it won't cause an endless loop
                                                            | baseSimplify q /= q   = baseSimplify $ Operator op p (baseSimplify q)
                                                            | otherwise         = e
baseSimplify e@(Operator op p@(Operator subop subex1 subex2) q)
                                                            | baseSimplify p /= p   = baseSimplify $ Operator op (baseSimplify p) q
                                                            | otherwise         = e
baseSimplify p@(Operator op a b) = p --again, so that no case continues indefinitely




{-simplifyAddition first extracts all the expressions to be added together into a list in stripPlus.  It then extracts all of the Numbers and adds them into one Number with gatherNumsPlus.
  Finally, it adds these elements of the list with addPlus, resulting in the form
  Operator Plus (Operator Plus (Operator Plus ... ) ) )
-}
simplifyAddition :: ArithExpr -> ArithExpr
simplifyAddition = addPlus . gatherNumsPlus . stripPlus

stripPlus :: ArithExpr -> [ArithExpr]
stripPlus (Operator Plus a b) = stripPlus a ++ stripPlus b
stripPlus expr = [expr]

addPlus :: [ArithExpr] -> ArithExpr
addPlus [x] = x
addPlus (x:xs) = Operator Plus (addPlus xs) x
        
gatherNumsPlus :: [ArithExpr] -> [ArithExpr]
gatherNumsPlus exprs = (Number (sum $ map (\(Number s) -> s) (filter isNum nums))) : notNums
    where
    notNums = filter (not . isNum) exprs
    nums = filter (isNum) exprs
    isNum (Number _) = True
    isNum _ = False
  
  
  
--I only implemented one function to baseSimplify multiplication, which multiplies together any Numbers in a sequence of products
simplifyMultiplication = gatherMult
 
gatherMult :: ArithExpr -> ArithExpr    

gatherMult (Operator Mult (Number a) (Number b)) = Number (a*b) --basic
gatherMult (Operator Mult (Operator Mult a (Number b)) (Number c)) = gatherMult $ Operator Mult a (Number $ b*c) --for cases where multiplying 3*x*2
gatherMult (Operator Mult (Operator Mult (Number a) b) (Number c)) = gatherMult $ Operator Mult b (Number $ a*c)
gatherMult (Operator Mult (Number a) (Operator Mult  (Number b) c)) = gatherMult $ Operator Mult c (Number $ a*b)
gatherMult (Operator Mult (Number a) (Operator Mult  b (Number c))) = gatherMult $ Operator Mult b (Number $ a*c)

gatherMult (Operator Mult p@(Operator Mult a b) c) --don't want to continue indefinitely, so check if gatherMult is still changing
                                                |gatherMult p /= p          = gatherMult $ Operator Mult (gatherMult p) c
                                                |otherwise                  = Operator Mult p c
gatherMult (Operator Mult c p@(Operator Mult a b))
                                                |gatherMult p /= p          = gatherMult $ Operator Mult (gatherMult p) c
                                                |otherwise                  = Operator Mult p c
gatherMult (Operator Plus a b) = Operator Plus (gatherMult a) (gatherMult b) --to break up added bits
gatherMult x = x


