    let e = ./emptySSHConfig.dhall

in  let AddKeysToAgent = constructors ./resources/AddKeysToAgent.dhall

in  [   e
      ⫽ { host =
            "*"
        , addKeysToAgent =
            Some (AddKeysToAgent.Yes {=})
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
            Some (AddKeysToAgent.No {=})
        , hostName =
            Some "server2.test"
        , port =
            Some 123
        , identityFile =
            Some "~/.ssh/other_id_rsa"
        }
    ]
