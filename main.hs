import Text.Read
import Data.List
import Data.Function
import Data.Tuple
import Data.Char

import Control.Arrow ((***))

type Variable = (Char, Int)

type Monomial = (Double, [Variable])

type Polynomial = [Monomial]

{- main :: IO ()
main = do
    putStrLn "Please insert the polynomial :"
    polynomial <- getLine
    putStrLn polynomial
    parsePolynomial polynomial -}

{- parsePolynomial :: String -> [String]
parsePolynomial (x:xs)
    | x `elem` list = parsePolynomial 
        where list = ['+', '-', '/', '*']
 -}

{- combine :: [(Int,Char)] -> [(Int, Char)]
combine = map (sum . map snd) . groupBy ((==) `on` snd) . sort -}

{- printPolynomial :: Polynomial -> [[Char]]
printPolynomial p =  [show a ++ show b ++ "^" ++ show c | (a,[b,c])<-p] -}


subsortGT :: Monomial -> Monomial -> Ordering
subsortGT a b
    | snd (head (snd a)) < snd(head (snd b)) = GT
    | snd (head (snd a)) > snd(head (snd b)) = LT
    | otherwise = sortGT (fst a, tail (snd a)) (fst b, tail(snd b))

sortGT :: Monomial -> Monomial -> Ordering
sortGT a b
    | null (snd a) && snd b /= [] = GT
    | null (snd b) && snd a /= [] = LT
    | null (snd a) && null (snd b) = LT
    | fst (head(snd a)) == fst (head(snd b)) = subsortGT a b
    | fst (head(snd a)) > fst (head(snd b)) = GT
    | fst (head(snd a)) < fst (head(snd b)) = LT


{- sortGT2 :: Monomial -> Monomial -> Ordering
sortGT2 a b
    | null (snd a) && snd b /= [] = GT
    | null (snd b) && snd a /= [] = LT
    | null (snd a) && null (snd b) = LT
    | fst (head(snd a)) == fst (head(snd b)) = subsortGT a b
    | [fst x | x<-snd a] > [fst x | x<-snd b] = GT
    | [fst x | x<-snd a] < [fst x | x<-snd b] = LT


subsortGT2 :: Monomial -> Monomial -> Ordering
subsortGT2 a b
    | [snd x | x<-snd a] <= [snd x | x<-snd b] = GT
    | otherwise = LT -}


sortExp :: Variable -> Variable -> Ordering
sortExp a b
    | fst a >= fst b = GT
    | fst a <= fst b = LT

{- safeGetExp :: Polynomial -> [(Char, Double)]
safeGetExp list = if null list then [] else snd (head list) -}

{- safeHead :: [a] -> a or Integer
safeHead list
    | null list = 1
    | otherwise = head list -}

{- simetricPolynomial :: Polynomial -> Polynomial
simetricPolynomial polynomial = [(-fst monomial, snd monomial) | monomial<-polynomial] -}

addPolynomials :: [Polynomial] -> Polynomial
addPolynomials list = [(sum (map fst x),  snd (head x)) | x<-groupBy ((==) `on` snd) (sortBy sortGT (concat list))]     -- acho melhor ser secundaria e chamar sempre a normalize polynomial para cada polynomial escrito pelo user e tambem para somar 2 polinomios

{- subtractPolynomials :: Polynomial -> Polynomial -> Polynomial
subtractPolynomials p1 p2 = [(sum (map fst m), snd (head m)) | m<-groupBy ((==) `on` snd) (sortBy sortGT (p1 ++ simetricPolynomial p2))] -}

multiplyMonomials :: Monomial -> Monomial -> Monomial
multiplyMonomials m1 m2 = (fst m1 * fst m2,[(fst (head m), sum (map snd m)) | m<- groupBy ((==) `on` fst) (sortBy sortExp (snd m1 ++ snd m2))])

multiplyPolynomials :: Polynomial -> Polynomial -> Polynomial
multiplyPolynomials p1 p2 =  [multiplyMonomials x y | x<- p1, y<- p2]

derivatePolynomial :: Polynomial -> Char -> Polynomial
derivatePolynomial p c = normalizePolynomial [derivateMonomial m c | m<-normalizePolynomial p]

derivateMonomial :: Monomial -> Char -> Monomial
derivateMonomial m c = (fst m * fromIntegral expChar, [if fst tup /= c then tup else (fst tup, snd tup - 1) | tup<-snd m])
        where expChar = getCharExp (snd m) c

getCharExp :: [Variable] -> Char -> Int
getCharExp [] c = 0
getCharExp (x:xs) c
    | fst x == c = snd x
    | otherwise = getCharExp xs c     
        

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial p = sortBy sortGT (filter checkNull (map cleanMonomial (addPolynomials [multiplyPolynomials [(1,[])] p])))   -- ordena / tira cenas do tipo "0*x^2" / converte cenas elevadas a 0 em vazio ex: 2*x*y^0 = 2*y ou 2*y^0 = 2 TODO SORT NOT WORKING

cleanMonomial :: Monomial -> Monomial
cleanMonomial p = (fst p, [x | x<-snd p, snd x /= 0])

checkNull :: Monomial -> Bool
checkNull m = fst m /= 0

----------Printing Functions----------

expToString :: Variable -> String  -- Variable -> String
expToString exp
    |snd exp == 1 = [fst exp]
    |otherwise = intercalate "^" [[fst exp], show (snd exp)]

monoToString :: Monomial -> String
monoToString m
    |fst m > 0 && fst m /= 1 && snd m /= [] = intercalate "+" [show(round (fst m)) ++ expToString exp | exp<-snd m]
    |fst m < 0 && fst m /= -1 && snd m /= [] = intercalate "" [show(round (fst m)) ++ expToString exp | exp<-snd m]
    |fst m == 1 && snd m /= [] = intercalate "+" [expToString exp | exp<-snd m]
    |fst m == -1 && snd m /= [] = intercalate "" ["-" ++ expToString exp | exp<-snd m]
    |fst m > 0 && snd m == [] = show(round (fst m))
    |fst m < 0 && snd m == [] = show(round (fst m))

printPoly :: Polynomial -> String
printPoly p = monoToString (head p) ++ intercalate "" [if fst m > 0 then "+" ++ monoToString m else monoToString m | m<-drop 1 p]

----------Parsing Functions----------

cutWhiteSpaces :: String -> String
cutWhiteSpaces = filter (\c-> c /= ' ' && ord c < 123)

parseCoefficient :: String -> [Double]
parseCoefficient s
    |s == [] = []
    |s /= [] && head s == '+' = if (takeWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (drop 1 s)) == [] then parseCoefficient (dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (drop 1 s)) else (read :: String -> Double) (takeWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (drop 1 s)) : parseCoefficient (dropWhile (\c -> ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c < 96 && ord c /= 45 && ord c /= 43) (drop 1 s)))
    |s /= [] && head s == '-' = (-(read :: String -> Double) (takeWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (drop 1 s))) : parseCoefficient (dropWhile (\c -> ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c < 96 && ord c /= 45 && ord c /= 43) (drop 1 s)))
    |s /= [] && (ord (head s) > 96 && ord (head s) < 123) = 1 : parseCoefficient (dropWhile (\c -> ord c /= 43 && ord c /= 45) s)
    |s /= [] && (ord (head s) < 96 && ord (head s) /= 94 && ord (head s) /= 43 && ord (head s) /= 45 ) = (read :: String -> Double) (takeWhile (\c -> ord c < 96 && (ord c /= 43 && ord c /= 45)) s) : parseCoefficient (dropWhile (\c -> ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c < 96 && ord c /= 45 && ord c /= 43) s))

parseVariable :: String -> [String]
parseVariable s
    |s == [] = []
    |s /= [] && (ord (head s) < 96 && ord (head s) /= 94) = if ((dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s)) == []) || (ord (head (dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s))) == 43 || ord (head (dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s))) == 45) then "V" : parseVariable (dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s)) else takeWhile  (\c -> ord c > 96 && ord c < 123) (dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s)) : parseVariable (dropWhile (\c -> ord c > 96 && ord c < 123) (dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s)))
    |s /= [] && (ord (head s) > 96 && ord (head s) < 123) = (takeWhile (\c -> ord c > 96 && ord c < 123) s) : parseVariable (dropWhile (\c -> ord c > 96 && ord c < 123) s)
    |s /= [] && (ord (head s) == 94) = parseVariable (dropWhile (\c -> ord c < 96 && (ord c /= 43 && ord c /= 45)) s)

parseExp :: String -> [Int]
parseExp s
    |s == [] = []
    |s /= [] && (ord (head s) < 96 && ord (head s) /= 94) = if ((dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s)) == []) || (ord (head (dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s))) == 43 || ord (head (dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s))) == 45) then 0 : parseExp (dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s)) else parseExp (dropWhile (\c -> ord c < 96 && ord c /= 43 && ord c /= 45) (dropWhile (\c -> ord c == 43 || ord c == 45) s))
    |s /= [] && (ord (head s) > 96 && ord (head s) < 123) = if (drop 1 s) == [] || (ord (head (drop 1 s)) > 96 && ord (head (drop 1 s)) < 123) || ord (head (drop 1 s)) == 43 || ord (head (drop 1 s)) == 45 then 1 : parseExp (drop 1 s) else parseExp (drop 1 s)
    |s /= [] && (ord (head s) == 94) = read (takeWhile (\c -> ord c < 96 && (ord c /= 43 && ord c /= 45)) (drop 1 s)) : parseExp (dropWhile (\c -> ord c < 96 && (ord c /= 43 && ord c /= 45)) (drop 1 s))

zipCoefVar :: String -> [(Double, Char)]
zipCoefVar s = [(fst tuple, c) | tuple<-zip (parseCoefficient s) (parseVariable s), c<-snd tuple]

zipCoefVarExp :: String -> [((Double, Char), Int)]
zipCoefVarExp s =  zip (zipCoefVar s) (parseExp s)

createPoly :: String -> Polynomial
createPoly s =  [if snd (fst m) /= 'V' then (fst (fst m), [(snd (fst m), snd m)]) else (fst (fst m), []) | m<-zipCoefVarExp (cutWhiteSpaces s)]