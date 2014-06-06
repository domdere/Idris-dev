module Prelude.Functor

import Prelude.Basics

||| Functors
||| @ f the action of the functor on objects
class Functor (f : Type -> Type) where
    ||| The action of the functor on morphisms
    ||| @ f the functor
    ||| @ m the morphism
    map : (m : a -> b) -> f a -> f b

||| Verified Functors
||| @ f the action of the functor on objects
class Functor f => VerifiedFunctor (f : Type -> Type) where
    ||| map must map the identity function on `a` to the identity function on `f a`
    total mapIdentity : (fa : f a) -> map id fa = fa

    ||| map must preserve function composition: map k . map g = map (k . g)
    total mapComposition : (fa : f a) -> (k : b -> c) -> (g : a -> b) -> (map k . map g) fa = map (k . g) fa
