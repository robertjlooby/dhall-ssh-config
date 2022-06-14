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

parseHostFields :: Map Text (Dhall.Core.RecordField s X) -> Either CompileError Text
parseHostFields fields =
  case Dhall.Core.recordFieldValue <$> Map.lookup "host" fields of
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
        Map.traverseWithKey parseHostRecordField (Map.delete "host" fields)
      return $ Map.foldMapWithKey (flip const) parsedFields

parseHostRecordField :: Text -> Dhall.Core.RecordField s X -> Either CompileError Text
parseHostRecordField f e = parseHostField f (Dhall.Core.recordFieldValue e)

parseHostField :: Text -> Expr s X -> Either CompileError Text
parseHostField f@"addKeysToAgent" e =
  parseEnumField f ["ask", "confirm", "no", "yes"] e
parseHostField f@"addressFamily" e = parseEnumField f ["any", "inet", "inet6"] e
parseHostField f@"batchMode" e = parseEnumField f ["no", "yes"] e
parseHostField f@"bindAddress" e = parseTextField f e
parseHostField f@"bindInterface" e = parseTextField f e
parseHostField f@"cASignatureAlgorithms" e = parseTextField f e
parseHostField f@"canonicalDomains" e = parseTextField f e
parseHostField f@"canonicalizeFallbackLocal" e =
  parseEnumField f ["no", "yes"] e
parseHostField f@"canonicalizeHostname" e =
  parseEnumField f ["always", "no", "yes"] e
parseHostField f@"canonicalizeMaxDots" e = parseNaturalField f e
parseHostField f@"canonicalizePermittedCNAMEs" e = parseTextField f e
parseHostField f@"certificateFile" e = parseTextField f e
parseHostField f@"challengeResponseAuthentication" e =
  parseEnumField f ["no", "yes"] e
parseHostField f@"checkHostIP" e = parseEnumField f ["no", "yes"] e
parseHostField f@"ciphers" e = parseTextField f e
parseHostField f@"clearAllForwardings" e = parseEnumField f ["no", "yes"] e
parseHostField f@"compression" e = parseEnumField f ["no", "yes"] e
parseHostField f@"connectTimeout" e = parseNaturalField f e
parseHostField f@"connectionAttempts" e = parseNaturalField f e
parseHostField f@"controlMaster" e =
  parseEnumField f ["auto", "autoask", "ask", "no", "yes"] e
parseHostField f@"controlPath" e = parseTextField f e
parseHostField f@"controlPersist" e = parseTextField f e
parseHostField f@"dynamicForward" e = parseTextField f e
parseHostField f@"enableSSHKeysign" e = parseEnumField f ["no", "yes"] e
parseHostField f@"escapeChar" e = parseTextField f e
parseHostField f@"exitOnForwardFailure" e = parseEnumField f ["no", "yes"] e
parseHostField f@"fingerprintHash" e = parseEnumField f ["md5", "sha256"] e
parseHostField f@"forwardAgent" e = parseEnumField f ["no", "yes"] e
parseHostField f@"forwardX11" e = parseEnumField f ["no", "yes"] e
parseHostField f@"forwardX11Timeout" e = parseTextField f e
parseHostField f@"forwardX11Trusted" e = parseEnumField f ["no", "yes"] e
parseHostField f@"gSSAPIAuthentication" e = parseEnumField f ["no", "yes"] e
parseHostField f@"gSSAPIDelegateCredentials" e =
  parseEnumField f ["no", "yes"] e
parseHostField f@"gatewayPorts" e = parseEnumField f ["no", "yes"] e
parseHostField f@"globalKnownHostsFile" e = parseTextField f e
parseHostField f@"hashKnownHosts" e = parseEnumField f ["no", "yes"] e
parseHostField f@"hostKeyAlgorithms" e = parseTextField f e
parseHostField f@"hostKeyAlias" e = parseTextField f e
parseHostField f@"hostName" e = parseTextField f e
parseHostField f@"hostbasedAuthentication" e = parseEnumField f ["no", "yes"] e
parseHostField f@"hostbasedKeyTypes" e = parseTextField f e
parseHostField f@"iPQoS" e = parseTextField f e
parseHostField f@"identitiesOnly" e = parseEnumField f ["no", "yes"] e
parseHostField f@"identityAgent" e = parseTextField f e
parseHostField f@"identityFile" e = parseTextField f e
parseHostField f@"ignoreUnknown" e = parseTextField f e
parseHostField f@"include" e = parseTextField f e
parseHostField f@"kbdInteractiveAuthentication" e =
  parseEnumField f ["no", "yes"] e
parseHostField f@"kbdInteractiveDevices" e = parseTextField f e
parseHostField f@"kexAlgorithms" e = parseTextField f e
parseHostField f@"localCommand" e = parseTextField f e
parseHostField f@"localForward" e = parseTextField f e
parseHostField f@"logLevel" e =
  parseEnumField
    f
    [ "DEBUG"
    , "DEBUG1"
    , "DEBUG2"
    , "DEBUG3"
    , "ERROR"
    , "FATAL"
    , "INFO"
    , "QUIET"
    , "VERBOSE"
    ]
    e
parseHostField f@"mACs" e = parseTextField f e
parseHostField f@"noHostAuthenticationForLocalhost" e =
  parseEnumField f ["no", "yes"] e
parseHostField f@"numberOfPasswordPrompts" e = parseNaturalField f e
parseHostField f@"pKCS11Provider" e = parseTextField f e
parseHostField f@"passwordAuthentication" e = parseEnumField f ["no", "yes"] e
parseHostField f@"permitLocalCommand" e = parseEnumField f ["no", "yes"] e
parseHostField f@"port" e = parseNaturalField f e
parseHostField f@"preferredAuthentications" e = parseTextField f e
parseHostField f@"proxyCommand" e = parseTextField f e
parseHostField f@"proxyJump" e = parseTextField f e
parseHostField f@"proxyUseFdpass" e = parseEnumField f ["no", "yes"] e
parseHostField f@"pubkeyAcceptedKeyTypes" e = parseTextField f e
parseHostField f@"pubkeyAuthentication" e = parseEnumField f ["no", "yes"] e
parseHostField f@"rekeyLimit" e = parseTextField f e
parseHostField f@"remoteCommand" e = parseTextField f e
parseHostField f@"remoteForward" e = parseTextField f e
parseHostField f@"requestTTY" e =
  parseEnumField f ["auto", "force", "no", "yes"] e
parseHostField f@"revokedHostKeys" e = parseTextField f e
parseHostField f@"sendEnv" e = parseTextField f e
parseHostField f@"serverAliveCountMax" e = parseNaturalField f e
parseHostField f@"serverAliveInterval" e = parseNaturalField f e
parseHostField f@"setEnv" e = parseTextField f e
parseHostField f@"streamLocalBindMask" e = parseTextField f e
parseHostField f@"streamLocalBindUnlink" e = parseEnumField f ["no", "yes"] e
parseHostField f@"strictHostKeyChecking" e =
  parseEnumField f ["accept-new", "ask", "no", "off", "yes"] e
parseHostField f@"syslogFacility" e =
  parseEnumField
    f
    [ "AUTH"
    , "DAEMON"
    , "LOCAL0"
    , "LOCAL1"
    , "LOCAL2"
    , "LOCAL3"
    , "LOCAL4"
    , "LOCAL5"
    , "LOCAL6"
    , "LOCAL7"
    , "USER"
    ]
    e
parseHostField f@"tCPKeepAlive" e = parseEnumField f ["no", "yes"] e
parseHostField f@"tunnel" e =
  parseEnumField f ["ethernet", "no", "point-to-point", "yes"] e
parseHostField f@"tunnelDevice" e = parseTextField f e
parseHostField f@"updateHostKeys" e = parseEnumField f ["ask", "no", "yes"] e
parseHostField f@"useKeychain" e = parseEnumField f ["no", "yes"] e
parseHostField f@"user" e = parseTextField f e
parseHostField f@"userKnownHostsFile" e = parseTextField f e
parseHostField f@"verifyHostKeyDNS" e = parseEnumField f ["ask", "no", "yes"] e
parseHostField f@"visualHostKey" e = parseEnumField f ["no", "yes"] e
parseHostField f@"xAuthLocation" e = parseTextField f e
parseHostField f _ = Left (CompileError $ "Unrecognized field \"" <> f <> "\"")

parseEnumField :: Text -> [Text] -> Expr s X -> Either CompileError Text
parseEnumField _ _ (Dhall.Core.App Dhall.Core.None _) = return ""
parseEnumField field options (Dhall.Core.Some e@(Dhall.Core.TextLit (Dhall.Core.Chunks [] t)))
  | t `elem` options = format field t
  | otherwise =
    typeError
      field
      ("Optional Text (one of: " <> fold (List.intersperse ", " options) <> ")")
      e
parseEnumField field options e =
  typeError
    field
    ("Optional Text (one of: " <> fold (List.intersperse ", " options) <> ")")
    e

parseNaturalField :: Text -> Expr s X -> Either CompileError Text
parseNaturalField _ (Dhall.Core.App Dhall.Core.None _) = return ""
parseNaturalField field (Dhall.Core.Some (Dhall.Core.NaturalLit n)) =
  format field $ Data.Text.pack (show n)
parseNaturalField field e = typeError field "Optional Natural" e

parseTextField :: Text -> Expr s X -> Either CompileError Text
parseTextField _ (Dhall.Core.App Dhall.Core.None _) = return ""
parseTextField field (Dhall.Core.Some (Dhall.Core.TextLit (Dhall.Core.Chunks [] t))) =
  format field t
parseTextField field e = typeError field "Optional Text" e

format :: Text -> Text -> Either CompileError Text
format field value = return $ "     " <> label <> " " <> value <> "\n"
  where
    firstChar = Data.Text.toUpper . Data.Text.take 1 $ field
    label = firstChar <> Data.Text.drop 1 field

typeError :: Text -> Text -> Expr s X -> Either CompileError Text
typeError field _type e =
  Left
    (CompileError $
     "The \"" <> field <> "\" field should be an " <> _type <>
     " value. Instead got " <>
     Dhall.Core.pretty e)
