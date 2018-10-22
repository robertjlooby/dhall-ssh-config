    let e = ./emptySSHConfig.dhall

in  [ e ⫽ { host = [ "*" ], user = Some "admin" }
    , e ⫽ { host = [ "server1" ], hostName = Some "server1.test" }
    ,   e
      ⫽ { host =
            [ "server2", "server2v2", "server2v3" ]
        , hostName =
            Some "server2.test"
        , port =
            Some 123
        }
    ]
