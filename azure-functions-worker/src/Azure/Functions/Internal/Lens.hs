module Azure.Functions.Internal.Lens
where

import Lens.Family

toEither :: Phantom f => a -> LensLike f (Maybe b) t (Either a b) b
toEither = to . orError
{-# INLINE toEither #-}

orError :: a -> Maybe b -> Either a b
orError msg = maybe (Left msg) Right
{-# INLINE orError #-}
