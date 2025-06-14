{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Spec where

import           ClassyPrelude              as P
import           Control.Exception
import           Data.Aeson
import           Data.Profunctor
import           Data.Row
import           Data.Row.Aeson             ()
import           Database.Surreal.Core
import           Effectful
import           Effectful.Concurrent
import           Effectful.Dispatch.Dynamic
import           Effectful.Error.Dynamic
import           Effectful.TH

main :: IO ()
main = putStrLn "Test suite not yet implemented"

newtype MyAppState
  = MyAppState { someOtherState :: Text }

data MyAppEffect :: Effect where
  GetState :: MyAppEffect m MyAppState
  LogMessage :: Text -> MyAppEffect m ()

makeEffect ''MyAppEffect

runMyAppEffectIO :: (IOE :> es) => Eff (MyAppEffect : es) a -> Eff es a
runMyAppEffectIO = interpret
  (\_ -> \case
      GetState -> return $ MyAppState "some dummy state"
      LogMessage t -> liftIO $ print t)

type AppEffs = [MyAppEffect, Surreal, Error RPCError, IOE]

runApp :: Eff AppEffs a -> IO a
runApp m = do
  eConn <- runEff $ runConcurrent $ runError $ connectRPC defaultConnectionInfo
  case eConn of
    Right cs -> do
      eR <- runEff $ runError $ runSurrealRPC cs $ runMyAppEffectIO m
      case eR of
        Right r -> return r
        Left a  -> handleErr a
    Left a -> handleErr a
  where
    handleErr :: (CallStack, RPCError) -> IO a
    handleErr (callStack, e) = do
      print e
      putStrLn $ pack $ prettyCallStack callStack
      throw e

array1 :: Eff AppEffs Value
array1 = query () [sql|return [1,2,3,4,5] :: Value |]

array2 :: Eff AppEffs Value
array2 = query () [sql|return [1,2,3,4,5][0] :: Value |]

array3 :: Eff AppEffs Value
array3 = query () [sql|return [1,2,3,4,5][0..] :: Value |]

array4 :: Eff AppEffs Value
array4 = query () [sql|return [1,2,3,4,5][0..] :: Value |]

array5 :: Eff AppEffs Value
array5 = query () [sql|
                      let $tmp = [1,2,3,4,5];
                      return $tmp[1..] :: Value;
                      |]

array6 :: Eff AppEffs Value
array6 = query () [sql|
                      let $tmp = [[1,2,3],[4,5,6]];
                      (return $tmp[0][1..]) :: Value;
                      |]
