{-# LANGUAGE OverloadedStrings #-}

module Dhall.SSHConfig
  ( dhallToSSHConfig
  ) where

import Data.Foldable (fold)
import Data.Sequence (intersperse)
import Data.Text (Text)
import qualified Data.Text
import Dhall.Core (Expr)
import qualified Dhall.Core
import Dhall.Map (Map)
import Dhall.Map as Map
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

parseHostFields :: Map Text (Expr s X) -> Either CompileError Text
parseHostFields fields =
  case Map.lookup "host" fields of
    Nothing -> Left (CompileError "Every configuration needs a \"host\" field.")
    Just (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)) ->
      (("Host " <> t <> "\n") <>) <$> body
    Just (Dhall.Core.ListLit _ hosts) -> do
      hosts' <- traverse getHost hosts
      body' <- body
      return $ "Host " <> fold (intersperse " " hosts') <> "\n" <> body'
    Just e ->
      Left
        (CompileError $
         "The \"host\" field should be a Text value or a List of Text values. Instead got " <>
         Dhall.Core.pretty e)
  where
    getHost :: Expr s X -> Either CompileError Text
    getHost (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)) = return t
    getHost e =
      Left
        (CompileError $
         "Values in the \"host\" list should be Text values. Instead got " <>
         Dhall.Core.pretty e)
    body = do
      parsedFields <-
        Map.traverseWithKey parseHostField (Map.delete "host" fields)
      return $ Map.foldMapWithKey (flip const) parsedFields

parseHostField :: Text -> Expr s X -> Either CompileError Text
parseHostField "hostName" (Dhall.Core.App Dhall.Core.None _) = return ""
parseHostField "hostName" (Dhall.Core.Some (Dhall.Core.TextLit (Dhall.Core.Chunks [] t))) =
  return ("     HostName " <> t <> "\n")
parseHostField "hostName" e =
  Left
    (CompileError $
     "The \"hostName\" field should be an Optional Text value. Instead got " <>
     Dhall.Core.pretty e)
parseHostField "port" (Dhall.Core.App Dhall.Core.None _) = return ""
parseHostField "port" (Dhall.Core.Some (Dhall.Core.NaturalLit n)) =
  return ("     Port " <> Data.Text.pack (show n) <> "\n")
parseHostField "port" e =
  Left
    (CompileError $
     "The \"port\" field should be an Optional Natural value. Instead got " <>
     Dhall.Core.pretty e)
parseHostField "user" (Dhall.Core.App Dhall.Core.None _) = return ""
parseHostField "user" (Dhall.Core.Some (Dhall.Core.TextLit (Dhall.Core.Chunks [] t))) =
  return ("     User " <> t <> "\n")
parseHostField "user" e =
  Left
    (CompileError $
     "The \"user\" field should be an Optional Text value. Instead got " <>
     Dhall.Core.pretty e)
parseHostField f _ = Left (CompileError $ "Unrecognized field \"" <> f <> "\"")
