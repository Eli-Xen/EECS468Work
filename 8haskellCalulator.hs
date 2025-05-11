--Haskel arithmatic parser and evaluator 
--inputs: string and chars
--outputs: single int 
--creation date: 4/25/25 


data Op = Add | Sub | Mul | Div | Mod | Exp deriving (Eq) -- Define all the operators 
instance Show Op where --new type and following defenitions 
    show Add = "+" --add is same as + 
    show Sub = "-" --Sub is same as - 
    show Mul = "*" --Mul is smae as * 
    show Div = "/" --Div is / 
    show Mod = "%" --Mod is %
    show Exp = "**" --Exp is ** 


data Token = Num Int | Op Op | LeftParen | RightParen | Start deriving (Eq) -- Define all tokens that can be parsed; Start is an added token from DOJ slides 
instance Show Token where --new type and follow its defenitions 
    show (Num n) = show n --Num is number 
    show (Op o) = show o --Op is operator o
    show LeftParen = "(" --left parenthesis 
    show RightParen = ")" --right parenthesis 
    show Start = "Start" --special token for when we want to signify this was the first call to a function that takes a token 

readInt :: String -> Int -- conversion string/char to int for parser 
readInt = read --does read from Prelude 

--function that takes a string and returns a list of parsed Tokens
parseTokens :: [Char] -> [Token] --parse tokens takes a list of chars and returns a list of Token types 
parseTokens [] = [] --base case, given empty list return empty 
parseTokens ('-' : xs) = Op Sub : parseTokens xs --if operator convert to proper operator, attach it to the result and recurse 
parseTokens ('+' : xs) = Op Add : parseTokens xs  --if operator convert to proper operator, attach it to the result and recurse 
parseTokens ('/' : xs) = Op Div : parseTokens xs --if operator convert to proper operator, attach it to the result and recurse 
parseTokens ('*' : '*' : xs) = Op Exp : parseTokens xs --if operator convert to proper operator, attach it to the result and recurse 
parseTokens ('*' : xs) = Op Mul : parseTokens xs --if operator convert to proper operator, attach it to the result and recurse 
parseTokens ('%' : xs) = Op Mod : parseTokens xs  --if operator convert to proper operator, attach it to the result and recurse 
parseTokens ('(' : xs) = LeftParen : parseTokens xs --if parenthesis convert to proper parenthesis, attach it to the result and recurse 
parseTokens (')' : xs) = RightParen : parseTokens xs--if parenthesis convert to proper parenthesis, attach it to the result and recurse 
parseTokens (x:xs) --for other characters 
    |x>='0' && x<='9'=Num(readInt[x]):parseTokens xs --if c is betwee '0'-'9' then convert it to integer and attach it to the parsed tokens and recurse 
    |x==' '=parseTokens xs --if x is a space skip it and recurse on 
    |otherwise=error ("Invalid Characters: unrecognized token starting with " ++ (x:xs)) --any other character calls an error 
parseTokens xs = error ("Invalid Characters: unrecognized token starting with " ++ xs) --anything else give error 

isDigitToken :: Token -> Bool --checks if token is bool 
isDigitToken (Num _) = True --if any number, whether single digit or not 
isDigitToken _       = False --if anything but a number 

combine :: Int -> Int -> Int --uses basic arithmetic to combine two digits into a number together 
combine a b = a*10+b --multiply first int by 10 and add the second one 5 and 5 --> 50+6=56 

parsePar :: ([Token], Int) -> Int --parenthesis counter, every ( adds 1 and ) subtracts 1, if counter 0 parenthesis are matched 
parsePar ([], n) = n --if given empty list return the parenthesis count 
parsePar ((LeftParen : stk), n) = parsePar (stk, n+1) --if ( add to count 
parsePar((RightParen : stk), n) = parsePar(stk, n-1) --if ) subtract from count 
parsePar ((x:stk), n) = parsePar(stk, n) --if anything else do nothing and recurse 

isOperator :: Token -> Bool --helper, checks if thing passed is an operator 
isOperator (Op _) = True -- if thing passed is any Operator type return True 
isOperator _ = False --anything else return false 

isLeftParen :: Token -> Bool --helper, checks if thing passed is a ( 
isLeftParen LeftParen = True --if thing passed is a ( return true 
isLeftParen _ = False --anything else return false 

parseNumbers :: [Token] -> Token -> [Token] --combine the number tokens representing digits and minus signs back into positive and negative numbers and also brings together multi-digit numbers; takes list of tokens and previous token and outputs tokens 
parseNumbers [] _ =[] --if empty do nothing; base case 

parseNumbers(Op Sub:Num n:tokens) Start=Num(-n):parseNumbers tokens (Num(-n)) --if we are starting the recurse through the list and we encounter a negative sign and then a number then add it to the number and recurse 
parseNumbers(Op Sub: Num n:tokens) prevToken --if we encounter a - and then a number, check the previous tokens...
    |isLeftParen prevToken || isOperator prevToken=Num(-n):parseNumbers tokens (Num(-n)) --if previous token was ( or an operator then make it a negtaive number and recurse 
    |otherwise= Op Sub:parseNumbers(Num n:tokens) (Op Sub) --otherwise just treat it as a minus sign and recurse 
    
parseNumbers(Num x:Num y:tokens) prevToken =parseNumbers (Num(combine x y):tokens) (Num (combine x y)) --two digits following eo, recurse with the combined digits and also pass the rest of the tokens 
parseNumbers (Num x:tokens) prevToken=Num x:parseNumbers tokens (Num x) --single digit, recurse to other tokens 
parseNumbers (x:tokens) prevToken =x:parseNumbers tokens x --any other token will be left alone and recurse 

precedence :: Op -> Int --helper function for precedence 
precedence Add = 1 --add,sub have lowest precedence 
precedence Sub = 1 --add,sub have lowest precedence 
precedence Mul = 2 --med priority 
precedence Div = 2 --med priority 
precedence Mod = 2 --med priority 
precedence Exp = 3 --exponent is highest priority 

shunt :: ([Token], [Token]) -> [Token] --shunting yard algo, takes list of tokens and the previous token, returns list of tokens reordered into postfix; uses stack to reorder from infix to postfix 
shunt ([],[])=[] --base case both stack and token list are empty 
shunt (stk,(Num x:xs))=Num x:shunt(stk,xs) --given stack and number, add number to output and recurse 
--shunt (Op o:stk,[]) = Op o:shunt(stk,[]) --just operator left and no more tokens
shunt(stk, LeftParen:xs)=shunt(LeftParen:stk,xs) --token is leftParenthesis ( push onto stack 
shunt(top:stk, RightParen:xs) --token is right parenthesis ), pop operators from stack until top of stack is left parenthesis ( 
    |top==LeftParen=shunt(stk,xs) --if top is left parenthesis then recurse to the rest of the stuff 
    |otherwise=top:shunt(stk,RightParen:xs) --add top of the stack to the output and recurse to the rest 
shunt(Op o1: stk, (Op o2):xs) --while theres an opeartor o2 at the top of the stack that isnt a left parenthesis, pop o2 from the stack into the output and push 01 onto stack 
    |precedence o1>=precedence o2=Op o1:shunt(stk, Op o2:xs) --if precedence of operator at the top of stack is greater than or same then add it on and recuse 
    |otherwise=shunt(Op o2:Op o1:stk, xs) --otherwise recuse passing top two operators of stack and the rest of the input 
shunt(stk, Op o: xs)=shunt(Op o:stk, xs) --if input current is operator then add it to stack and recurse 
shunt(top:stk, [])= top:shunt(stk,[]) -- Token list is empty: Pop remaining items from the operator stack to output 

apply :: Op -> Float -> Float -> Float -- Function to apply operators  
apply Add x y = x + y --Add does + 
apply Sub x y = x - y --Sub does - 
apply Mul x y = x * y --Mul does * 
apply Div x y = x / y --Div does / 
apply Mod x y = x - (fromIntegral(floor (x/y)) * y) --Mod performs the mod sequence 
apply Exp x y = x ** y --Exp does exponentiation 

rpn :: ([Float], [Token]) -> Float --takes an intial value and the list of tokens and returns a calculated result 
rpn ([x], []) = x --if initial is just a list and empty list return whats inside the list 
rpn (stk, (Num n : xs)) = rpn ((fromIntegral n : stk), xs) --if second param is a num at the top then recurse and call fromIntegral on the num 
rpn ((0:x:stk), (Op Div : xs))=error("Division b 0") --if top two elements are a num and 0 and the operator is a Div then error bc division by 0 
rpn ((x : y : stk), (Op o : xs)) = rpn ((apply o y x : stk), xs) --if two nums and an operator then call apply to perform the operation 
rpn((x : []), (Op o:xs))=error("operator without operand" ++ show o) --if nothing left and given an operatior then that means theres an extra operator and we throw error 
rpn((x), (xs)) = error("Missing operator") --if given nums without anything at the end then were missing an operator so throw error 

parse :: [Char] -> Float --takes a list of chars and then passes it through the parser, shunt, and rpn to give result, error checks at beggining to make sure parenthesis match 
parse xs = if parsePar (a, 0) == 0 --if parenthesis checker returns 0 that means that we have the all matching parenthesis... 
           then rpn ([], shunt ([], a)) --and we call shunt and rpn 
           else error("Unmatched Parenthesis") --otherwise call error 
      where a = parseNumbers (parseTokens xs) Start --gives all parameters to each function, ie shunt and RPN need an empty list as input too 

--main :: IO ()
--main = do
    --putStrLn "Enter an expression:"
    --input <- getLine
    --print (parse input)

