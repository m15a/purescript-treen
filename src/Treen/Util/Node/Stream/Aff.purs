module Treen.Util.Node.Stream.Aff
  ( fromString
  ) where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Node.Buffer (Buffer)
import Node.Buffer (fromString) as B
import Node.Encoding (Encoding)

-- | Encode a string as an array of buffer with the given encoding.
fromString :: forall m. MonadEffect m => Encoding -> String -> m (Array Buffer)
fromString encoding s = liftEffect $ map pure $ B.fromString s encoding
