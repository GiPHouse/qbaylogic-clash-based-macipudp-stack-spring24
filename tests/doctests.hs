module Main where

import Build_doctests ( flags, module_sources, pkgs )
import Prelude
import Data.Maybe (isJust)
import System.Environment ( lookupEnv )
import System.Process
import Test.DocTest ( doctest )

getGlobalPackageDb :: IO String
getGlobalPackageDb = readProcess "ghc" ["--print-global-package-db"] ""

main :: IO ()
main = do
  inNixShell <-lookupEnv "IN_NIX_SHELL"
  inNixDocker <- lookupEnv "IN_NIX_DOCKER"

  extraFlags <-
    if isJust inNixShell || isJust inNixDocker
      then pure . ("-package-db="++) <$> getGlobalPackageDb
      else pure []

  let
    pluginFlags =
      [ "-fplugin", "GHC.TypeLits.KnownNat.Solver"
      , "-fplugin", "GHC.TypeLits.Normalise"
      , "-fplugin", "GHC.TypeLits.Extra.Solver" ]

  doctest (flags ++ extraFlags ++ pkgs ++ pluginFlags ++ module_sources)
