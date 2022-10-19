import Text.Read
import Data.List
import Data.Function

import Data.Tuple

import Control.Arrow ((***))

type Monomial = (Double, [(Char, Double)])

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


sortExp :: (Char,Double) -> (Char,Double) -> Ordering
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
derivateMonomial m c = (fst m * expChar, [if fst tup /= c then tup else (fst tup, snd tup - 1) | tup<-snd m])
        where expChar = getCharExp (snd m) c
getCharExp :: [(Char, Double)] -> Char -> Double
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

expToString :: (Char, Double) -> String
expToString exp
    |snd exp == 1 = [fst exp]
    |otherwise = intercalate "^" [[fst exp], show (round(snd exp))]

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