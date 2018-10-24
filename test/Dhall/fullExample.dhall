    let e = ./resources/EmptySSHConfig.dhall

in  [   e
      ⫽ { host =
            "*"
        , addKeysToAgent =
            Some "yes"
        , identityFile =
            Some "~/.ssh/id_rsa"
        , user =
            Some "admin"
        }
    , e ⫽ { host = "server1", hostName = Some "server1.test" }
    ,   e
      ⫽ { host =
            "server2"
        , addKeysToAgent =
            Some "no"
        , hostName =
            Some "server2.test"
        , port =
            Some 123
        , identityFile =
            Some "~/.ssh/other_id_rsa"
        }
    ]
