module Utils
  ( Decorated
  , Decorator (..)
  , decorate
  , unDecorate
  , mapD
  , runD
  , mkD
  , WithDefault (..)
  , maybeDefault
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

-- Decorators for characters. May be kind of useless.

-- These will later be extended with something about their order
-- in the list, and a better ID type. For now, use the ident field
-- for description. Yes, that's weird.
data Decorator a = Decorator
  { ident :: Text
  -- , desc :: Text -- Maybe ?
  , fun :: a -> a
  }

data Decorated a = Decorated
  { raw :: a
  , computed :: a -- This shit is useless. TODO
  , decoration :: [Decorator a]
  }

mkD :: a -> [Decorator a] -> Decorated a
mkD d ds = mapDecorators (const ds) $ Decorated d d []

mapD :: (a -> b) -> Decorated a -> b
mapD f = f . computed

runD :: Decorated a -> a
runD = mapD id

addDecorator :: Decorator a -> [Decorator a] -> [Decorator a]
addDecorator d ds = d:ds -- TODO

mapDecorators :: ([Decorator d] -> [Decorator d]) -> Decorated d -> Decorated d
mapDecorators f d =
  let deco = f $ decoration d
  in d { computed = foldl (.) id (map fun deco) (raw d) -- TODO
       , decoration = deco
       }

decorate :: Decorated d -> Decorator d -> Decorated d
decorate = flip $ mapDecorators . addDecorator

unDecorate :: (Decorator d -> Bool) -> Decorated d -> Decorated d
unDecorate f = mapDecorators $ filter (not . f)

{-
instance Monad m => Monad (DecoratorT d m) where return x = DecoratorT $ return x
  m >>= f = DecoratorT $ runDecoratorT m >>= runDecoratorT . f

instance Monad m => Functor (DecoratorT d m) where
  fmap f xs = xs >>= return . f

instance Monad m => MonadReader d (DecoratorT d m)  where
  ask = DecoratorT $ gets computed >>= lift
  local f m = DecoratorT $
              get >>= \save ->
              lift (liftM f $ computed save) >>= \nval ->
              put (Decorated { raw = nval
                             , computed = return nval
                             , decoration = [] }) >>
              runDecoratorT m >>= \res ->
              put save >> return res

instance MonadTrans (DecoratorT d) where
  lift = DecoratorT . lift

instance MonadRandom m => MonadRandom (DecoratorT d m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs
  -}

class WithDefault a where
  defaultValue :: a

instance WithDefault (Map k a) where
  defaultValue = Map.empty

maybeDefault :: WithDefault a => Maybe a -> a
maybeDefault (Just x) = x
maybeDefault Nothing = defaultValue
