{-# LANGUAGE OverloadedStrings #-}

module Dhall.SSHConfig
  ( dhallToSSHConfig
  ) where

import Data.Foldable (fold)
import Data.List as List
import Data.Sequence as Seq
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
        Right s -> return $ fold $ Seq.intersperse "\n" s
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
      return $ "Host " <> fold (Seq.intersperse " " hosts') <> "\n" <> body'
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
parseHostField "addKeysToAgent" e =
  parseEnumField
    "addKeysToAgent"
    "AddKeysToAgent"
    ["ask", "confirm", "no", "yes"]
    e
parseHostField "hostName" e = parseTextField "hostName" "HostName" e
parseHostField "identityFile" e = parseTextField "identityFile" "IdentityFile" e
parseHostField "port" e = parseNaturalField "port" "Port" e
parseHostField "useKeychain" e =
  parseEnumField "useKeychain" "UseKeychain" ["no", "yes"] e
parseHostField "user" e = parseTextField "user" "User" e
parseHostField f _ = Left (CompileError $ "Unrecognized field \"" <> f <> "\"")

parseEnumField :: Text -> Text -> [Text] -> Expr s X -> Either CompileError Text
parseEnumField _ _ _ (Dhall.Core.App Dhall.Core.None _) = return ""
parseEnumField field label options (Dhall.Core.Some e@(Dhall.Core.TextLit (Dhall.Core.Chunks [] t)))
  | t `elem` options = format label t
  | otherwise =
    typeError
      field
      ("Optional Text (one of: " <> fold (List.intersperse ", " options) <> ")")
      e
parseEnumField field _ options e =
  typeError
    field
    ("Optional Text (one of: " <> fold (List.intersperse ", " options) <> ")")
    e

parseNaturalField :: Text -> Text -> Expr s X -> Either CompileError Text
parseNaturalField _ _ (Dhall.Core.App Dhall.Core.None _) = return ""
parseNaturalField _ label (Dhall.Core.Some (Dhall.Core.NaturalLit n)) =
  format label $ Data.Text.pack (show n)
parseNaturalField field _ e = typeError field "Optional Natural" e

parseTextField :: Text -> Text -> Expr s X -> Either CompileError Text
parseTextField _ _ (Dhall.Core.App Dhall.Core.None _) = return ""
parseTextField _ label (Dhall.Core.Some (Dhall.Core.TextLit (Dhall.Core.Chunks [] t))) =
  format label t
parseTextField field _ e = typeError field "Optional Text" e

format :: Text -> Text -> Either CompileError Text
format label value = return $ "     " <> label <> " " <> value <> "\n"

typeError :: Text -> Text -> Expr s X -> Either CompileError Text
typeError field _type e =
  Left
    (CompileError $
     "The \"" <> field <> "\" field should be an " <> _type <>
     " value. Instead got " <>
     Dhall.Core.pretty e)
