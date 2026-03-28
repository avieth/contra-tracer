{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Control.Tracer (traceWith)
import Test.Control.Tracer.Properties
  ( laziness_1
  , laziness_2
  , laziness_3
  , traceTraversable_laziness
  , traceAll_laziness)

assertNoException :: String -> IO a -> IO ()
assertNoException name action = do
  result <- try action
  case result of
    Left (e :: SomeException) ->
      error ("FAILED: " ++ name ++ "\n  unexpected exception: " ++ show e)
    Right _ ->
      putStrLn ("PASS: " ++ name)

main :: IO ()
main = do
  assertNoException "laziness_1" $
    traceWith laziness_1 (error "failure")

  assertNoException "laziness_2" $
    traceWith laziness_2 (error "failure")

  assertNoException "laziness_3" $
    traceWith laziness_3 False

  assertNoException "traceTraversable_laziness" $
    traceWith traceTraversable_laziness (error "failure")

  assertNoException "traceAll_laziness" $
    traceWith traceAll_laziness True
