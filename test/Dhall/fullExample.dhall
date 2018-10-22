    let e = ./emptySSHConfig.dhall

in  [   e
      ⫽ { host = "*", identityFile = Some "~/.ssh/id_rsa", user = Some "admin" }
    , e ⫽ { host = "server1", hostName = Some "server1.test" }
    ,   e
      ⫽ { host =
            "server2"
        , hostName =
            Some "server2.test"
        , port =
            Some 123
        , identityFile =
            Some "~/.ssh/other_id_rsa"
        }
    ]
