{-# LANGUAGE OverloadedStrings #-}

module Dhall.SSHConfigSpec
  ( spec
  ) where

import qualified Data.Text.IO
import qualified Dhall
import Dhall.SSHConfig
import Test.Hspec
  ( Expectation
  , Spec
  , describe
  , expectationFailure
  , it
  , shouldBe
  )

expectFailure :: Dhall.Text -> Expectation
expectFailure input = do
  expr <- Dhall.inputExpr input
  case dhallToSSHConfig expr of
    Right t -> expectationFailure ("Expected failure. Got: " <> show t)
    Left _ -> return ()

shouldConvertTo :: Dhall.Text -> Dhall.Text -> Expectation
shouldConvertTo input output = do
  expr <- Dhall.inputExpr input
  dhallToSSHConfig expr `shouldBe` Right output

spec :: Spec
spec = do
  describe "dhallToSSHConfig" $ do
    describe "empty configs" $ do
      it "for an empty config" $ "[] : List {host : Text}" `shouldConvertTo` ""
      it "for the wrong top level element" $ expectFailure "{=}"
    describe "the host config" $ do
      it "for a record without a host config" $
        expectFailure "[{hostName = [\"1.2.3.4\"]}]"
      it "for a host value other than text" $ expectFailure "[{host = 2}]"
      it "for a single host config" $
        "[{host = \"test\"}]" `shouldConvertTo` "Host test\n"
      it "for multiple host configs" $
        "[{host = \"test\"}, {host = \"other\"}]" `shouldConvertTo`
        "Host test\n\nHost other\n"
    describe "the hostName config" $ do
      it "for a hostName value other than optional text" $
        expectFailure "[{host = \"test\", hostName = 1234}]"
      it "for a single hostName config" $
        "[{host = \"test\", hostName = [\"1.2.3.4\"] : Optional Text}]" `shouldConvertTo`
        "Host test\n     HostName 1.2.3.4\n"
    it "handles a full example config" $ do
      dhall <-
        Data.Text.IO.readFile "./test/Dhall/fullExample.dhall" >>=
        Dhall.inputExpr
      sshConfig <- Data.Text.IO.readFile "./test/Dhall/fullExample"
      dhallToSSHConfig dhall `shouldBe` Right sshConfig
  it "the empty config is valid Dhall and doesn't add any configuration" $
    "[./emptySSHConfig.dhall // {host = \"test\"}]" `shouldConvertTo`
    "Host test\n"
