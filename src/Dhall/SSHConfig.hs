{-# LANGUAGE OverloadedStrings #-}

module Dhall.SSHConfig
  ( dhallToSSHConfig
  ) where

import Data.Foldable (fold)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd as InsOrd
import Data.Sequence (intersperse)
import Data.Text (Text)
import qualified Data.Text
import Dhall.Core (Expr)
import qualified Dhall.Core
import Dhall.TypeCheck (X)

newtype CompileError =
  CompileError Text
  deriving (Eq)

instance Show CompileError where
  show (CompileError err) =
    Data.Text.unpack $
    _ERROR <> ": Cannot convert to SSH configuration\n\\↳ " <> err

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

dhallToSSHConfig :: Expr s X -> Either CompileError Text
dhallToSSHConfig e0 = parseList (Dhall.Core.normalize e0)

parseList e =
  case e of
    Dhall.Core.ListLit _ a ->
      case traverse parseHost a of
        Left e -> Left e
        Right s -> return $ fold $ intersperse "\n" s
    _ ->
      Left
        (CompileError $
         "The top level expression should be a List. Instead got " <>
         Dhall.Core.pretty e)

parseHost :: Expr s X -> Either CompileError Text
parseHost e =
  case e of
    Dhall.Core.RecordLit fields -> parseHostFields fields
    _ ->
      Left
        (CompileError $
         "Each host configuration should be a Record. Instead got " <>
         Dhall.Core.pretty e)

parseHostFields :: InsOrdHashMap Text (Expr s X) -> Either CompileError Text
parseHostFields fields =
  case InsOrd.lookup "host" fields of
    Nothing -> Left (CompileError "Every configuration needs a \"host\" field.")
    Just (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)) ->
      (("Host " <> t <> "\n") <>) <$>
      InsOrd.foldlWithKey'
        parseHostField
        (return "")
        (InsOrd.delete "host" fields)
    Just e ->
      Left
        (CompileError $
         "The \"host\" field should be a Text value. Instead got " <>
         Dhall.Core.pretty e)

parseHostField ::
     Either CompileError Text -> Text -> Expr s X -> Either CompileError Text
parseHostField (Left e) _ _ = Left e
parseHostField (Right acc) "hostName" (Dhall.Core.OptionalLit _ Nothing) =
  return acc
parseHostField (Right acc) "hostName" (Dhall.Core.OptionalLit _ (Just (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)))) =
  return (acc <> "     HostName " <> t <> "\n")
parseHostField _ "hostName" e =
  Left
    (CompileError $
     "The \"hostName\" field should be an Optional Text value. Instead got " <>
     Dhall.Core.pretty e)
parseHostField (Right acc) "user" (Dhall.Core.OptionalLit _ Nothing) =
  return acc
parseHostField (Right acc) "user" (Dhall.Core.OptionalLit _ (Just (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)))) =
  return (acc <> "     User " <> t <> "\n")
parseHostField _ "user" e =
  Left
    (CompileError $
     "The \"user\" field should be an Optional Text value. Instead got " <>
     Dhall.Core.pretty e)
parseHostField _ f _ =
  Left (CompileError $ "Unrecognized field \"" <> f <> "\"")
