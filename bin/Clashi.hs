import Clash.Main (defaultMain)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = getArgs >>= defaultMain . ("--interactive" :)
