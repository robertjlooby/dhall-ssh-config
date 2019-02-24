    let SSHConfig = ./resources/SSHConfig.dhall ⩓ { host : Text }

in  let e = ./resources/EmptySSHConfig.dhall

in  let baseConfig =
            e
          ⫽ { host =
                "*"
            , addKeysToAgent =
                Some "yes"
            , identityFile =
                Some "~/.ssh/id_rsa"
            , useKeychain =
                Some "yes"
            }

in  let user1Servers =
          [ { host = "FirstServer", hostName = "first-server.test" }
          , { host = "SecondServer", hostName = "second-server.example" }
          ]

in  let user2Servers =
          [ { host = "ThirdServer", hostName = "third-server.test" }
          , { host = "FourthServer", hostName = "fourth-server.example" }
          ]

in  let buildConfig =
            λ(user : Text)
          → λ(config : { host : Text, hostName : Text })
          → λ(configs : List SSHConfig)
          →   [   e
                ⫽ { host =
                      config.host
                  , hostName =
                      Some config.hostName
                  , user =
                      Some user
                  }
              ]
            # configs

in  let user1Config =
          List/fold
          { host : Text, hostName : Text }
          user1Servers
          (List SSHConfig)
          (buildConfig "User1")
          ([] : List SSHConfig)

in  let user2Config =
          List/fold
          { host : Text, hostName : Text }
          user2Servers
          (List SSHConfig)
          (buildConfig "User2")
          ([] : List SSHConfig)

in  [ baseConfig ] # user1Config # user2Config
