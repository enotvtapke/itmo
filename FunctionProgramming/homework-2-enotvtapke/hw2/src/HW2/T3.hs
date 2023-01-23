module HW2.T3
  ( joinOption,
    joinExcept,
    joinAnnotated,
    joinList,
    joinFun,
  )
where

import HW2.T1

joinOption :: Option (Option a) -> Option a
joinOption None     = None
joinOption (Some x) = x

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)   = Error e
joinExcept (Success x) = x

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e1 <> e2)

concatList :: List a -> List a -> List a
concatList Nil l      = l
concatList (a :. b) l = a :. concatList b l

joinList :: List (List a) -> List a
joinList Nil      = Nil
joinList (a :. b) = concatList a $ joinList b

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\x -> let F g = f x in g x)
