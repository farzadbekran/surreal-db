{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}

module Database.Surreal.MonadSurreal where

import           ClassyPrelude          hiding ( error, id )
import           Control.Monad.Catch
import           Data.Aeson             as J
import           Data.Profunctor
import           Database.Surreal.TH
import           Database.Surreal.Types

class MonadThrow m => MonadSurreal m where
  getNextRequestID :: m Int
  send :: Text -> [Value] -> m Response
  runQuery :: input -> Query input (Either DecodeError output) -> m output
  registerLiveListener :: Text -> (LiveResponse -> IO ()) -> m ()
  unregisterLiveListener :: Text -> m ()
