{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
{- |
Module      :  Control.Category.Cont
Description :  Provides a type for Continuation Passing Style development
Copyright   :  (c) Matteo Provenzano 2015

License     :  BSD-style (see the LICENSE file in the distribution)
Maintainer  :  matteo.provenzano@alephdue.com
Stability   :  experimental
Portability :  portable
-}

module Control.Category.Cont ( -- * The Cont category
                               -- $Category

                               -- ** Category laws
                               -- $Laws
                               Cont
                               -- * Utility functions
                             , forget
                             , withCont
                             , lift
                             , cont
                             -- * Example: Composable sorter
                             -- $Example1
                             
                             -- * Example: Composable actions
                             -- $Example2

                             -- * Example: Monoid
                             -- $Example3
                             ) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Monoid

{-$Category
The Continuation category is defined as follow:

- object are functions of the type @f :: a -> b@, @g :: c -> d@.
- arrows are functions of the type @t :: (a -> b) -> (c -> d)@.
- the identity @'id'@ is the function that takes a function f and returns the same function.
- the composition @.@ operator takes two functions
 @t1 :: (a -> b) -> (c -> d)@, @t2 :: (c -> d) -> (e -> f)@ and returns the function @t :: (a -> b) -> (e -> f)@.
-}

{-$Laws
The category laws are trivially verified:

- Identity law:
@'Cont' f . 'Cont' 'id' = 'Cont' f . 'id' = 'Cont' 'f' = 'Cont' 'id' . f = 'Cont' 'id' . 'Cont' f@
- Associativity law:
@('Cont' f . 'Cont' g) . 'Cont' h = 'Cont' (f . g) . 'Cont' h = 'Cont' (f . g . h) = 'Cont' (f . (g . h)) = 'Cont' f . 'Cont' (g .h) = 'Cont' f . ('Cont' g . 'Cont' h)@
-}

-- |A type for the Continuation category.
newtype Cont f g = Cont (f -> g)

instance Category Cont where
    (Cont f) . (Cont g) = Cont (f . g )
    id = Cont id

instance Monoid a => Monoid (Cont t (f -> a)) where
    Cont f `mappend` Cont g = Cont $ \h x -> f h x `mappend` g h x
    mempty = Cont $ \h x -> mempty

-- |Creates a continuation
cont :: (f -> g) -> Cont f g
cont f = Cont f

-- |Forgets the continuation.
forget :: Cont (a -> a) (b -> c) -> b -> c
forget (Cont f) = f id

-- |Apply a function to the continuation.
withCont :: (b -> c) -> Cont (a -> b) (a -> c)
withCont f = Cont $ \g -> f . g

-- |Lift the continuation into a Monad.
lift :: Monad m => Cont (a -> b) (a -> m b)
lift = withCont return

{-$Example1
Here is an example how to use the @Cont@ category to compose a custom sorter:

>import Prelude hiding (id, (.))
>import Control.Category
>import Control.Category.Cont
>import Data.List
>
>data User = User { name :: String
>                  , surname :: String
>                  , yob :: Int
>                  } deriving Show
>
>users = [ User { name = "Amadeus", surname = "Mozart", yob = 1756 }
>        , User { name = "Amadeus", surname = "Brahms", yob = 1833 }
>        , User { name = "Johannes", surname = "Brahms", yob = 1833 }
>        , User { name = "Johannes", surname = "Mozart", yob = 1833 }
>        , User { name = "Antonio", surname = "Vivaldi", yob = 1678 }
>        , User { name = "Antonio", surname = "Vivaldi", yob = 1679 }
>        ]
>
>order = cont $ \f x -> sortBy (curry f) x 
>by field = cont $
>   \f x -> if ord x == EQ then
>              f x
>           else
>              ord x
>        where
>           ord x = compare ((field . fst) x) ((field . snd) x)
>
>eqOtherwise = cont $ \f x -> EQ
>
>mysort = forget $ order . (by surname) . (by name) . (by yob) . eqOtherwise
-}
{-$Example2
Here is an example how to combine the Cont category with the @IO@ monad:

>import Prelude hiding (id, (.))
>import Control.Category
>import Control.Category.Cont
>
>withPassword pwd = cont $ \f x -> do
>    putStrLn "Enter the secret password:"
>    pass <- getLine
>    if pass == pwd then
>        f x
>    else
>        return "you are not authorized to execute this action."
>
>greet = cont $ \f x -> f $ "hello to " ++ x
>
>secureGreet = forget $ (withPassword "secret") . lift . greet
>verySecureGreet = forget $ (withPassword "secret") . (withPassword "verySecret") . lift . greet

The action @withPassword@ requests the user to enter a string. If the string matches the password, the input is handed to the continuation.
@lift@ is used to inject the pure code into the IO monad.
-}
{-$Example3
>import Prelude hiding (id, (.))
>import Control.Category
>import Control.Category.Cont
>
>ins a = [a]
>select op = cont $
>    \f x -> f $ op x
>
>toList :: (a, a) -> [a]
>toList = forget $  (select fst `mappend` select snd) . (withCont ins)
-}