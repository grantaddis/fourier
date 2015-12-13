module FSeries where

import Data.Char
import Data.List

data Term = Constant [String] | Ratio [String] [String] | TrigValue Term TrigFunction Term

data TrigFunction = Sin | Cos | MSin | MCos deriving (Show)

data PolyTrig = Poly [(Term, Int)] | Trig Term TrigFunction Term

prodIntConcatString :: String -> (Int, [String]) -> (Int, [String])
prodIntConcatString v (i, s) = case reads v :: [(Int, String)] of
  [(n, "")] -> (i * n, s)
  _ -> (i, s ++ [v])

repToExp :: [String] -> String
repToExp e = foldr (\s -> let p = (length . filter (==s) $ e) in
                     (++ (case p of
                                1 -> s
                                _ -> "(" ++ s ++ "^" ++ (show p) ++ ")"))) "" (nub e)

simplifyExp :: [String] -> String
simplifyExp s = case (t, repToExp v) of
  (1, []) -> "1"
  (1, vs) -> vs
  (_, vs) -> (show t) ++ vs
  where (t, v) = foldr (prodIntConcatString) (1, []) s

showTerm :: Term -> String
showTerm t = case t of
  Constant xs -> case (length . filter (=="0") $ xs) > 0 of
    True -> "0"
    False -> simplifyExp xs
  Ratio xs ys -> case (length . filter (=="0") $ xs) > 0 of
    True -> "0"
    False -> "(" ++ (simplifyExp xs) ++ "/" ++ (simplifyExp ys) ++ ")"
  TrigValue c f x -> case (showTerm c) of
    "0" -> "0"
    _ -> (let sinx = (case (showTerm c, showTerm x) of
                       ("0", _) -> "0"
                       (_, "0") -> "0"
                       (cs, xs) -> cs ++ "sin(" ++ xs ++ ")")
              cosx = (case (showTerm c, showTerm x) of
                       ("0", _) -> "0"
                       (cs, "0") -> cs
                       (cs, xs) -> cs ++ "cos(" ++ xs ++ ")") in
          (case f of
            Sin -> sinx
            Cos -> cosx
            MSin -> "-" ++ sinx
            MCos -> "-" ++ cosx))
      

showPolyTrig :: PolyTrig -> String
showPolyTrig pt = case pt of
  Poly ts -> foldr (++) "" $
             map (\(c, e) -> "(" ++ (showTerm c) ++ ", " ++ (show e) ++ ")") ts
  Trig c f w -> (showTerm c) ++ (show f) ++ (showTerm w)

showSolution :: [([Term], [Term])] -> String
showSolution [] = ""
showSolution ((xp, xt):(yp, yt):zs) = "(" ++ (intercalate " + " (map showTerm xp)) ++ ")(" ++
                                      (showTerm (head xt)) ++ ") - (" ++
                                      (intercalate " + " (map showTerm yp)) ++ ")(" ++
                                      (showTerm (head yt)) ++ ")" ++
                                      (case zs of
                                        [] -> ""
                                        _ -> " + " ++ showSolution zs)
showSolution s = intercalate " + " $ map (\(p, t) ->
      "(" ++ (intercalate " + " (map showTerm p)) ++ ")(" ++ (showTerm (head t)) ++ ")") s

multPolyTerms :: Term -> Term -> Term
multPolyTerms x y = case (x, y) of
  (Constant s, Constant r) -> Constant (s ++ r)
  (Constant s, Ratio rn rd) -> Ratio (s ++ rn) rd
  (Ratio sn sd, Constant r) -> Ratio (sn ++ r) sd
  (Ratio sn sd, Ratio rn rd) -> Ratio (sn ++ rn) (sd ++ rd)

exponentiateTerm :: Term -> Int -> Term
exponentiateTerm x p = case p of
  0 -> Constant ["1"]
  _ ->
    case x of
    Constant xs -> Constant $ foldr (\_ -> (++ xs)) [] [1..p]
    Ratio xn xd -> Ratio (foldr (\_ -> (++ xn)) [] [1..p])
                   (foldr (\_ -> (++ xd)) [] [1..p])

invertTerm :: Term -> Term
invertTerm t = case t of
  Constant xs -> Ratio ["1"] xs
  Ratio xn xd -> Ratio xd xn

evaluatePolyComponent :: (Term, Int) -> Term -> Term
evaluatePolyComponent (c, e) x =
  multPolyTerms c (exponentiateTerm x e)

evaluatePolyTrig :: PolyTrig -> Term -> [Term]
evaluatePolyTrig f x = case f of
  Poly ts -> map (\c -> evaluatePolyComponent c x) ts
  Trig c f w -> [TrigValue c f (multPolyTerms x w)]

differentiatePolyComponent :: (Term, Int) -> (Term, Int)
differentiatePolyComponent (c, e) = (multPolyTerms c (Constant [show e]), e - 1)

differentiatePoly :: PolyTrig -> PolyTrig
differentiatePoly (Poly f) = Poly $ map (differentiatePolyComponent)
                      (filter (\(_, e) -> e > 0) f)

antiDifferentiateTrig :: PolyTrig -> PolyTrig
antiDifferentiateTrig (Trig c f w) = Trig rp fp w
  where rp = multPolyTerms c (invertTerm w)
        fp = case f of
          Sin -> MCos
          Cos -> Sin
          MSin -> Cos
          MCos -> MSin

integrate :: [PolyTrig] -> Term -> Term -> [([Term], [Term])]
integrate [f, t] a b = case (f, t) of
  ((Poly []), _) -> []
  (_, _) -> [((evaluatePolyTrig f b), (evaluatePolyTrig (antiDifferentiateTrig t) b))] ++
            [((evaluatePolyTrig f a), (evaluatePolyTrig (antiDifferentiateTrig t) a))] ++
            (integrate [(differentiatePoly f), (antiDifferentiateTrig t)] a b)

test :: Int -> String
test i = case i of
  1 -> showSolution $ integrate [Poly [((Constant ["3"]), 2), ((Constant ["4"]), 1)], Trig (Constant ["1"]) Sin (Constant ["p"])] (Constant ["a"]) (Constant ["b"])
  2 -> showSolution $ integrate [Poly [((Constant ["-A"]), 2), ((Constant ["A", "L"]), 1)], Trig (Constant ["1"]) Sin (Ratio ["n", "pi"] ["L"])] (Constant ["0"]) (Constant ["L"])
