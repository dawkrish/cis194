module Party where

import Data.Tree
import Employee

-- Exercise 1.1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ s) (GL es score) = GL (es ++ [e]) (score + s)

-- Exercise 1.2
instance Semigroup GuestList where
  (<>) (GL xs x) (GL ys y) = GL (xs ++ ys) (x + y)

instance Monoid GuestList where
  mempty = GL [] 0

-- Exercise 1.3
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
-- treeFold :: ([b] -> a -> b) -> b -> Tree a  -> b
-- treeFold f acc (Node label []) = acc
-- treeFold f acc (Node label forest) =

treeFold :: (a -> b) -> (b -> b -> b) -> Tree a  -> b
treeFold f g  (Node lab forest) = foldl (\acc t -> g acc (treeFold f g  t)) (f lab) forest

treeSum = treeFold (\(Emp _ x) -> x) (+)
treeNames = treeFold (\(Emp x _) -> [x]) (++)
treeNames' = treeFold (\(Emp x _) -> x) (++)

-- eeFold' ::(b -> a -> [b] -> b) -> Tree a -> b -> b
-- treeFold' f (Node lab forest) e = foldl (\acc t -> f acc lab   ) (f e lab []) forest

-- treeFold f (Node lab forest) e = foldl (\acc t -> acc) e forest

{-
  f (treeFold f acc (forest!! 0)) acc f (treeFold f acc (forest))
-}

-- -- Exercise 3
-- nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- nextLevel boss results = mconcat []
--   where
--     xs = [(better gl1, better gl2) | (gl1, gl2) <- results]
--     better gl = moreFun (glCons boss gl)

-- -- Exercise 4
-- maxFun :: Tree Employee -> GuestList
-- maxFun  = treeFold go (GL [] 0)
--   where
--     go gl@(GL es stfu) boss@(Emp n s) = gl
