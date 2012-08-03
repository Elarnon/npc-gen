module Generic.Feat
  ( Feat (..)
  ) where

-- TODO: implement feats in a similar way to Class

newtype Feat = Feat String
  deriving (Eq, Show, Ord)
