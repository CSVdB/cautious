{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Cautious.CautiousT where

import Import

import Cautious.Cautious

import Control.Monad.Trans.Class

newtype CautiousT w e (m :: * -> *) a = CautiousT
    { runCautiousT :: m (Cautious w e a)
    } deriving (Generic, Functor)

cautiousWarning :: Monad m => w -> a -> CautiousT w e m a
cautiousWarning w a = CautiousT . pure $ CautiousWarning w a

cautiousWarningIfNothing ::
       (Monoid w, Monad m) => w -> Maybe a -> CautiousT w e m (Maybe a)
cautiousWarningIfNothing w Nothing = cautiousWarning w Nothing
cautiousWarningIfNothing _ (Just a) = cautiousWarning mempty $ Just a

cautiousWarningM :: Monad m => w -> m a -> CautiousT w e m a
cautiousWarningM w ma = CautiousT $ CautiousWarning w <$> ma

cautiousError :: Monad m => e -> CautiousT w e m a
cautiousError e = CautiousT . pure $ CautiousError e

cautiousErrorIfNothing ::
       (Monoid w, Monad m) => Maybe a -> e -> CautiousT w e m a
cautiousErrorIfNothing Nothing e = CautiousT . pure $ CautiousError e
cautiousErrorIfNothing (Just a) _ = pure a

instance (Applicative m, Monoid w) => Applicative (CautiousT w e m) where
    pure = CautiousT . pure . pure
    CautiousT mf <*> CautiousT ma = CautiousT $ liftA2 (<*>) mf ma

instance (Monad m, Monoid w) => Monad (CautiousT w e m) where
    CautiousT ma >>= f =
        CautiousT $ do
            ra <- ma
            case ra of
                CautiousWarning w a -> do
                    rb <- runCautiousT $ f a
                    case rb of
                        CautiousWarning w' b ->
                            pure $ CautiousWarning (w <> w') b
                        CautiousError e -> pure $ CautiousError e
                CautiousError e -> pure $ CautiousError e
    a >> b = a *> b

instance (MonadIO m, Monoid w) => MonadIO (CautiousT w e m) where
    liftIO f = CautiousT $ pure <$> liftIO f

instance Monoid w => MonadTrans (CautiousT w e) where
    lift ma = CautiousT $ CautiousWarning mempty <$> ma

instance Validity (m (Cautious e w a)) => Validity (CautiousT e w m a) where
    validate = validate . runCautiousT

instance Show (m (Cautious e w a)) => Show (CautiousT e w m a) where
    show = show . runCautiousT

instance Eq (m (Cautious e w a)) => Eq (CautiousT e w m a) where
    CautiousT x == CautiousT y = x == y
