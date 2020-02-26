let SSHConfig = ./resources/SSHConfig.dhall ⩓ { host : Text }

let e = ./resources/EmptySSHConfig.dhall

let baseConfig =
        e
      ⫽ { host = "*"
        , addKeysToAgent = Some "yes"
        , identityFile = Some "~/.ssh/id_rsa"
        , useKeychain = Some "yes"
        }

let user1Servers =
      [ { host = "FirstServer", hostName = "first-server.test" }
      , { host = "SecondServer", hostName = "second-server.example" }
      ]

let user2Servers =
      [ { host = "ThirdServer", hostName = "third-server.test" }
      , { host = "FourthServer", hostName = "fourth-server.example" }
      ]

let buildConfig =
        λ(user : Text)
      → λ(config : { host : Text, hostName : Text })
      → λ(configs : List SSHConfig)
      →   [   e
            ⫽ { host = config.host
              , hostName = Some config.hostName
              , user = Some user
              }
          ]
        # configs

let user1Config =
      List/fold
        { host : Text, hostName : Text }
        user1Servers
        (List SSHConfig)
        (buildConfig "User1")
        ([] : List SSHConfig)

let user2Config =
      List/fold
        { host : Text, hostName : Text }
        user2Servers
        (List SSHConfig)
        (buildConfig "User2")
        ([] : List SSHConfig)

in  [ baseConfig ] # user1Config # user2Config
