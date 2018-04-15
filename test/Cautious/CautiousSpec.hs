{-# LANGUAGE TypeApplications #-}

module Cautious.CautiousSpec
    ( spec
    ) where

import TestImport

import Cautious.Cautious
import Cautious.CautiousT
import Cautious.Gen ()

type CautiousExample = Cautious String String

type CautiousExampleT = CautiousT String String Maybe

spec :: Spec
spec = do
    genValidSpec @(CautiousExample Int)
    eqSpec @(CautiousExample Int)
    functorSpecOnValid @CautiousExample
    applicativeSpecOnValid @CautiousExample
    monadSpecOnValid @CautiousExample
    functorSpecOnValid @CautiousExampleT
    applicativeSpecOnValid @CautiousExampleT
    monadSpecOnValid @CautiousExampleT
