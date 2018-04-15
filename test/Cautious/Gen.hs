{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cautious.Gen where

import TestImport

import Cautious.Cautious
import Cautious.CautiousT

instance (GenUnchecked a, GenUnchecked w, GenUnchecked e) =>
         GenUnchecked (Cautious w e a)

instance (GenValid a, GenValid w, GenValid e) => GenValid (Cautious w e a)

instance GenUnchecked (m (Cautious e w a)) =>
         GenUnchecked (CautiousT e w m a)

instance GenValid (m (Cautious e w a)) => GenValid (CautiousT e w m a)
