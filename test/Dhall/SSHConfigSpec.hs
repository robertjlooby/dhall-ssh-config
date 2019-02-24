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
        expectFailure "[{hostName = Some \"1.2.3.4\"}]"
      it "for a host value other than text" $ expectFailure "[{host = 2}]"
      it "for multiple host value other than text" $
        expectFailure "[{host = [1, 2]}]"
      it "for a single host config" $
        "[{host = \"test\"}]" `shouldConvertTo` "Host test\n"
      it "for multiple host configs" $
        "[{host = \"test\"}, {host = \"other\"}]" `shouldConvertTo`
        "Host test\n\nHost other\n"
      it "for configs with multiple hosts" $
        "[{host = [\"test\", \"test2\"]}, {host = [\"other\", \"other2\"]}]" `shouldConvertTo`
        "Host test test2\n\nHost other other2\n"
    describe "an enum config" $ do
      it "for a value other than optional text" $
        expectFailure "[{host = \"test\", addKeysToAgent = 1234}]"
      it "for a single value in the enum" $
        "[{host = \"test\", addKeysToAgent = Some \"ask\"}]" `shouldConvertTo`
        "Host test\n     AddKeysToAgent ask\n"
      it "for a value not in the enum" $
        expectFailure "[{host = \"test\", addKeysToAgent = Some \"other\"}]"
    describe "a natural config" $ do
      it "for a value other than optional natural" $
        expectFailure "[{host = \"test\", port = -1234}]"
      it "for a single optional natural value" $
        "[{host = \"test\", port = Some 123}]" `shouldConvertTo`
        "Host test\n     Port 123\n"
    describe "a text config" $ do
      it "for a value other than optional text" $
        expectFailure "[{host = \"test\", hostName = 1234}]"
      it "for a single optional text value" $
        "[{host = \"test\", hostName = Some \"1.2.3.4\"}]" `shouldConvertTo`
        "Host test\n     HostName 1.2.3.4\n"
    it "handles a full example config" $ do
      dhall <-
        Data.Text.IO.readFile "./test/Dhall/fullExample.dhall" >>=
        Dhall.inputExpr
      sshConfig <- Data.Text.IO.readFile "./test/Dhall/fullExample"
      dhallToSSHConfig dhall `shouldBe` Right sshConfig
    it "handles a full example config with multiple hosts per config" $ do
      dhall <-
        Data.Text.IO.readFile "./test/Dhall/fullExampleMultipleHosts.dhall" >>=
        Dhall.inputExpr
      sshConfig <- Data.Text.IO.readFile "./test/Dhall/fullExampleMultipleHosts"
      dhallToSSHConfig dhall `shouldBe` Right sshConfig
    it "handles an example config with functions" $ do
      dhall <-
        Data.Text.IO.readFile "./test/Dhall/functionExample.dhall" >>=
        Dhall.inputExpr
      sshConfig <- Data.Text.IO.readFile "./test/Dhall/functionExample"
      dhallToSSHConfig dhall `shouldBe` Right sshConfig
  it "the empty config is valid Dhall and doesn't add any configuration" $
    "[./resources/EmptySSHConfig.dhall // {host = \"test\"}]" `shouldConvertTo`
    "Host test\n"
