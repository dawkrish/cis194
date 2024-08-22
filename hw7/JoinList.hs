module JoinList where

import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Show, Eq)

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

getSizeTag :: (Sized m, Monoid m) => JoinList m a -> Int
getSizeTag = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ x) = Nothing
indexJ i l@(Append m l1 l2)
  | i < 0 || i >= size0 = Nothing
  | i < size1 = indexJ i l1
  | otherwise = indexJ (i - size1) l2
  where
    size0 = getSizeTag l
    size1 = getSizeTag l1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 l = l
dropJ _ Empty = Empty
dropJ n s@(Single _ _) = s
dropJ n l@(Append m l1 l2)
  | n >= size0 = Empty
  | n < size1 = dropJ n l1 +++ l2
  | otherwise = dropJ (n - size1) l2
  where
    size0 = getSizeTag l
    size1 = getSizeTag l1
