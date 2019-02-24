let e = ./resources/EmptySSHConfig.dhall

in  [   e
      ⫽ { host =
            [ "*" ]
        , addKeysToAgent =
            Some "yes"
        , identityFile =
            Some "~/.ssh/id_rsa"
        , useKeychain =
            Some "yes"
        , user =
            Some "admin"
        }
    , e ⫽ { host = [ "server1" ], hostName = Some "server1.test" }
    ,   e
      ⫽ { host =
            [ "server2", "server2v2", "server2v3" ]
        , addKeysToAgent =
            Some "no"
        , addressFamily =
            Some "any"
        , batchMode =
            Some "yes"
        , bindAddress =
            Some "localhost"
        , bindInterface =
            Some "test"
        , cASignatureAlgorithms =
            Some "test"
        , canonicalDomains =
            Some "test"
        , canonicalizeFallbackLocal =
            Some "no"
        , canonicalizeHostname =
            Some "always"
        , canonicalizeMaxDots =
            Some 123
        , canonicalizePermittedCNAMEs =
            Some "test"
        , certificateFile =
            Some "test"
        , challengeResponseAuthentication =
            Some "yes"
        , checkHostIP =
            Some "no"
        , ciphers =
            Some "test"
        , clearAllForwardings =
            Some "yes"
        , compression =
            Some "no"
        , connectTimeout =
            Some 123
        , connectionAttempts =
            Some 123
        , controlMaster =
            Some "autoask"
        , controlPath =
            Some "test"
        , controlPersist =
            Some "test"
        , dynamicForward =
            Some "test"
        , enableSSHKeysign =
            Some "yes"
        , escapeChar =
            Some "test"
        , exitOnForwardFailure =
            Some "no"
        , fingerprintHash =
            Some "md5"
        , forwardAgent =
            Some "no"
        , forwardX11 =
            Some "no"
        , forwardX11Timeout =
            Some "test"
        , forwardX11Trusted =
            Some "no"
        , gSSAPIAuthentication =
            Some "no"
        , gSSAPIDelegateCredentials =
            Some "no"
        , gatewayPorts =
            Some "no"
        , globalKnownHostsFile =
            Some "test"
        , hashKnownHosts =
            Some "no"
        , hostKeyAlgorithms =
            Some "test"
        , hostKeyAlias =
            Some "test"
        , hostName =
            Some "server2.test"
        , hostbasedAuthentication =
            Some "no"
        , hostbasedKeyTypes =
            Some "test"
        , iPQoS =
            Some "test"
        , identitiesOnly =
            Some "no"
        , identityAgent =
            Some "test"
        , identityFile =
            Some "~/.ssh/other_id_rsa"
        , ignoreUnknown =
            Some "test"
        , include =
            Some "test"
        , kbdInteractiveAuthentication =
            Some "yes"
        , kbdInteractiveDevices =
            Some "test"
        , kexAlgorithms =
            Some "test"
        , localCommand =
            Some "test"
        , localForward =
            Some "test"
        , logLevel =
            Some "QUIET"
        , mACs =
            Some "test"
        , noHostAuthenticationForLocalhost =
            Some "no"
        , numberOfPasswordPrompts =
            Some 123
        , pKCS11Provider =
            Some "test"
        , passwordAuthentication =
            Some "no"
        , permitLocalCommand =
            Some "no"
        , port =
            Some 123
        , preferredAuthentications =
            Some "test"
        , proxyCommand =
            Some "test"
        , proxyJump =
            Some "test"
        , proxyUseFdpass =
            Some "no"
        , pubkeyAcceptedKeyTypes =
            Some "test"
        , pubkeyAuthentication =
            Some "no"
        , rekeyLimit =
            Some "test"
        , remoteCommand =
            Some "test"
        , remoteForward =
            Some "test"
        , requestTTY =
            Some "force"
        , revokedHostKeys =
            Some "test"
        , sendEnv =
            Some "test"
        , serverAliveCountMax =
            Some 123
        , serverAliveInterval =
            Some 123
        , setEnv =
            Some "test"
        , streamLocalBindMask =
            Some "test"
        , streamLocalBindUnlink =
            Some "no"
        , strictHostKeyChecking =
            Some "accept-new"
        , syslogFacility =
            Some "DAEMON"
        , tCPKeepAlive =
            Some "no"
        , tunnel =
            Some "point-to-point"
        , tunnelDevice =
            Some "test"
        , updateHostKeys =
            Some "ask"
        , useKeychain =
            Some "no"
        , userKnownHostsFile =
            Some "test"
        , verifyHostKeyDNS =
            Some "ask"
        , visualHostKey =
            Some "yes"
        , xAuthLocation =
            Some "test"
        }
    ]
