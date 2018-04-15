{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Cautious.Cautious where

import Import

data Cautious w e a
    = CautiousWarning w
                      a
    | CautiousError e
    deriving (Generic, Functor)

instance Monoid w => Applicative (Cautious w e) where
    pure = CautiousWarning mempty
    (<*>) (CautiousWarning w f) (CautiousWarning w2 a) =
        CautiousWarning (mappend w w2) $ f a
    (<*>) (CautiousError e) _ = CautiousError e
    (<*>) _ (CautiousError e) = CautiousError e

instance Monoid w => Monad (Cautious w e) where
    (>>=) (CautiousWarning w a) f =
        case f a of
            CautiousWarning w2 x -> CautiousWarning (mappend w w2) x
            CautiousError e -> CautiousError e
    (>>=) (CautiousError e) _ = CautiousError e

instance (Validity a, Validity w, Validity e) => Validity (Cautious w e a)

instance (Show w, Show e, Show a) => Show (Cautious w e a) where
    show (CautiousWarning w a) =
        "CautiousWarnings: " ++ show w ++ "\nResult: " ++ show a
    show (CautiousError e) = "CautiousError: " ++ show e

instance (Eq w, Eq e, Eq a) => Eq (Cautious w e a) where
    (==) (CautiousWarning w a) (CautiousWarning w' a') = w == w' && a == a'
    (==) (CautiousError e) (CautiousError e') = e == e'
    _ == _ = False
