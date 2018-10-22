    let AddKeysToAgent = ./resources/AddKeysToAgent.dhall

in  { addKeysToAgent =
        None AddKeysToAgent
    , hostName =
        None Text
    , identityFile =
        None Text
    , port =
        None Natural
    , user =
        None Text
    }
