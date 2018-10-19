module Main where

import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.SSHConfig
import qualified GHC.IO.Encoding
import qualified System.Exit
import qualified System.IO

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  stdin <- Data.Text.IO.getContents
  dhall <- Dhall.inputExpr stdin
  case Dhall.SSHConfig.dhallToSSHConfig dhall of
    Right sshConfig -> Data.Text.IO.putStr sshConfig
    Left e -> do
      System.IO.hPrint System.IO.stderr e
      System.Exit.exitFailure
