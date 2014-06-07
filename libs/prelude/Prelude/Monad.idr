module Prelude.Monad

-- Monads and Functors

import Builtins
import Prelude.List
import Prelude.Applicative

%access public

infixl 5 >>=

class Applicative m => Monad (m : Type -> Type) where
    (>>=)  : m a -> (a -> m b) -> m b

infixr 1 >=>

||| Kleisli Composition
(>=>) : Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g x = f x >>= g

infixr 1 <=<

(<=<) : Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) = flip (>=>)

||| Also called `join` or mu
flatten : Monad m => m (m a) -> m a
flatten a = a >>= id

||| For compatibility with Haskell. Note that monads are **not** free to
||| define `return` and `pure` differently!
return : Monad m => a -> m a
return = pure

liftM2 : Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f mx my = mx >>= (\x => my >>= (\y => pure (f x y)))

class Monad m => VerifiedMonad (m : Type -> Type) where
    total monadPureIdentityL : (k : a -> m b) -> (x : a) -> pure x >>= k = k x
    total monadPureIdentityR : (ma : m a) -> ma >>= pure = ma
    total monadBindAssociative : (k : a -> m b) -> (h : b -> m c) -> (ma : m a) -> ma >>= (k >=> h) = (ma >>= k) >>= h

    ||| Lifting a curried function using the Monad bind should be consistent with the Applicative behaviour
    total monadBindApplySame : (f : a -> b -> c) -> (mx : m a) -> (my : m b) -> liftM2 f mx my = liftA2 f mx my

