{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, FunctionalDependencies #-}
module Leibniz
  ( refl
  , subst
  , tran
  , symm
  , inj
  , tyCon
  ) where

import Data.GADT.Compare

-- Fun stuff with equality

-- Lowercase reflexivity for consistency
refl :: a := a
refl = Refl

-- Leibniz's substitution principle
subst :: a := b -> f a -> f b
subst Refl = id

-- Transitivity
-- We use the fact that a := u is ((:=) a) u and the substitution principle
tran :: a := u -> u := b -> a := b
tran au ub = subst ub au

-- Using a type function and the substitution principle to show symmetry
newtype FS a b = FS { unFS :: b := a }

symm :: a := b -> b := a
symm equ = unFS . subst equ . FS $ Refl

-- Now, we show injectivity

-- Type family to deconstruct first argument
type family Arg a :: *
type instance Arg (f a) = a

-- Type function to apply the substitution principle
newtype ArgA a b = ArgA { unArgA :: Arg a := Arg b }

-- Injectivity
inj :: f a := f' b -> a := b
inj eq = unArgA . subst eq $ ra
  where 
    ra :: ArgA (f a) (f a)
    ra = ArgA Refl

-- Type constructor application
tyCon :: a := b -> f a := f b
tyCon Refl = Refl
