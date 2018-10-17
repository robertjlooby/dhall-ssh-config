    let e = ./emptySSHConfig.dhall

in  [ e ⫽ { host = "*", user = [ "admin" ] : Optional Text }
    , e ⫽ { host = "server1", hostName = [ "server1.test" ] : Optional Text }
    , e ⫽ { host = "server2", hostName = [ "server2.test" ] : Optional Text }
    ]
