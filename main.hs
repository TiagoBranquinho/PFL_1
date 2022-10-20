import Data.List
import Data.Function
import Data.Char

----------Defining Types----------


type Variable = (Char, Int)

type Monomial = (Double, [Variable])

type Polynomial = [Monomial]

----------Functions for Users------

addPolynomials :: String -> String -> String
addPolynomials p1 p2 = printPolynomial (getNormalizedPolynomial (getAddPolynomials (createPolynomial p1) (createPolynomial p2)))

multiplyPolynomials :: String -> String -> String
multiplyPolynomials p1 p2 = printPolynomial (getNormalizedPolynomial (getMultiplyPolynomials (createPolynomial p1) (createPolynomial p2)))

derivatePolynomial :: String -> Char -> String
derivatePolynomial p1 c = printPolynomial (getNormalizedPolynomial (getDerivatePolynomial (createPolynomial p1) c))

normalizePolynomial :: String -> String
normalizePolynomial p1 = printPolynomial (getNormalizedPolynomial (createPolynomial p1))


----------Sorting Functions------

sortGT :: Monomial -> Monomial -> Ordering   -- Sorts a Polynomial, selecting first the ones that have a minor 'Variable' letter

sortGT a b
    | null (snd a) && snd b /= [] = GT
    | null (snd b) && snd a /= [] = LT
    | null (snd a) && null (snd b) = LT
    | fst (head(snd a)) == fst (head(snd b)) = subsortGT a b
    | fst (head(snd a)) > fst (head(snd b)) = GT
    | fst (head(snd a)) < fst (head(snd b)) = LT

subsortGT :: Monomial -> Monomial -> Ordering -- In case of draw in the above function, select first the ones where the expoent of the 'Variable' is higher

subsortGT a b
    | snd (head (snd a)) < snd(head (snd b)) = GT
    | snd (head (snd a)) > snd(head (snd b)) = LT
    | otherwise = sortGT (fst a, tail (snd a)) (fst b, tail(snd b))


sortExp :: Variable -> Variable -> Ordering -- Sorts a list of 'Variable' of a 'Monomial', selecting first the ones that have a minor 'Variable' letter

sortExp a b
    | fst a >= fst b = GT
    | fst a <= fst b = LT

----------Main Functions----------

{- simetricPolynomial :: Polynomial -> Polynomial
simetricPolynomial polynomial = [(-fst monomial, snd monomial) | monomial<-polynomial] -}

getAddPolynomials :: Polynomial -> Polynomial -> Polynomial         -- Returns a 'Polynomial' which is the sum of 2 'Polynomials'

getAddPolynomials p1 p2 = [(sum (map fst x),  snd (head x)) | x<-groupBy ((==) `on` snd) (sortBy sortGT (p1 ++ p2))]

{- subtractPolynomials :: Polynomial -> Polynomial -> Polynomial
subtractPolynomials p1 p2 = [(sum (map fst m), snd (head m)) | m<-groupBy ((==) `on` snd) (sortBy sortGT (p1 ++ simetricPolynomial p2))] -}

getMultiplyMonomials :: Monomial -> Monomial -> Monomial             -- Returns a 'Monomial' which is the multiplication of 2 'Monomials'

getMultiplyMonomials m1 m2 = (fst m1 * fst m2,[(fst (head m), sum (map snd m)) | m<- groupBy ((==) `on` fst) (sortBy sortExp (snd m1 ++ snd m2))])

getMultiplyPolynomials :: Polynomial -> Polynomial -> Polynomial     -- Returns a 'Polynomial' which is the multiplication of 2 'Polynomials'

getMultiplyPolynomials p1 p2 =  [getMultiplyMonomials x y | x<- p1, y<- p2]

getDerivatePolynomial :: Polynomial -> Char -> Polynomial            -- Returns the derivative of a 'Polynomial'

getDerivatePolynomial p c = [derivateMonomial m c | m<-getNormalizedPolynomial p]

derivateMonomial :: Monomial -> Char -> Monomial                  -- Returns the derivative of a 'Monomial'

derivateMonomial m c = (fst m * fromIntegral expChar, [if fst tup /= c then tup else (fst tup, snd tup - 1) | tup<-snd m])
        where expChar = getCharExp (snd m) c

getCharExp :: [Variable] -> Char -> Int                          -- Returns the expoent of a 'Variable' if present in a list, else 0

getCharExp [] c = 0
getCharExp (x:xs) c
    | fst x == c = snd x
    | otherwise = getCharExp xs c     
        

getNormalizedPolynomial :: Polynomial -> Polynomial                  -- Normalizes a 'Polynomial', removing null elements and 'Variables' equal to 1

getNormalizedPolynomial p = sortBy sortGT (filter checkNull (map cleanMonomial (getAddPolynomials (getMultiplyPolynomials [(1,[])] p) [])))

cleanMonomial :: Monomial -> Monomial                     -- Removes 'Variables' equal to 1 from a 'Monomial'

cleanMonomial p = (fst p, [x | x<-snd p, snd x /= 0])           

checkNull :: Monomial -> Bool                                   -- Condition to filter the list of 'Monomials', removing the ones equal to 0

checkNull m = fst m /= 0

----------Printing Functions----------

expToString :: Variable -> String  

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

printPolynomial :: Polynomial -> String

printPolynomial p = monoToString (head p) ++ intercalate "" [if fst m > 0 then "+" ++ monoToString m else monoToString m | m<-drop 1 p]

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

createPolynomial :: String -> Polynomial

createPolynomial s =  [if snd (fst m) /= 'V' then (fst (fst m), [(snd (fst m), snd m)]) else (fst (fst m), []) | m<-zipCoefVarExp (cutWhiteSpaces s)]