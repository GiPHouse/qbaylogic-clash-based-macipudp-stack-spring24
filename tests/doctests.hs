module Main where

import Build_doctests ( flags, module_sources, pkgs )
import Prelude
import System.Environment ( lookupEnv )
import System.Process
import Test.DocTest ( doctest )

getGlobalPackageDb :: IO String
getGlobalPackageDb = readProcess "ghc" ["--print-global-package-db"] ""

main :: IO ()
main = do
  inNixShell <-lookupEnv "IN_NIX_SHELL"
  extraFlags <-
    case inNixShell of
      Nothing -> pure []
      Just _ -> pure . ("-package-db="++) <$> getGlobalPackageDb

  let
    pluginFlags =
      [ "-fplugin", "GHC.TypeLits.KnownNat.Solver"
      , "-fplugin", "GHC.TypeLits.Normalise"
      , "-fplugin", "GHC.TypeLits.Extra.Solver"
      , "-fconstraint-solver-iterations=10" ]

  doctest (flags ++ extraFlags ++ pkgs ++ pluginFlags ++ module_sources)
