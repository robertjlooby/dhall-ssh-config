    let e = ./resources/EmptySSHConfig.dhall

in  [   e
      ⫽ { host =
            "*"
        , addKeysToAgent =
            Some "yes"
        , identityFile =
            Some "~/.ssh/id_rsa"
        , useKeychain =
            Some "yes"
        , user =
            Some "admin"
        }
    , e ⫽ { host = "server1", hostName = Some "server1.test" }
    ,   e
      ⫽ { host =
            "server2"
        , addKeysToAgent =
            Some "no"
        , addressFamily =
            Some "any"
        , batchMode =
            Some "yes"
        , hostName =
            Some "server2.test"
        , port =
            Some 123
        , identityFile =
            Some "~/.ssh/other_id_rsa"
        , useKeychain =
            Some "no"
        }
    ]
