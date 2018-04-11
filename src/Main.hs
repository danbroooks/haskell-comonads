{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude
import Control.Comonad

data Free f a = Pure a
              | Bind (f (Free f a))

instance Functor f => Monad (Free f) where
  return a = Pure a
  Pure a >>= f = f a
  Bind m >>= f = Bind (fmap (>>= f) m)

add :: Int -> Int -> Int
add a b = a + b

main :: IO ()
main = putStrLn $ show $ add 2 2
