# dhall-ssh-config

This is a tool for generating an ssh config file using the [Dhall](https://dhall-lang.org/) configuration language.

### Why?

The Dhall site linked above explains some benefits of the language in general. The reasons to use Dhall + `dhall-ssh-config` for generating your ssh config in particular are:

* Functions: This is the main one. Use functions to DRY up your config.
* Type checking: Some limited type checking (though most fields are just `Text` values)
* Enum checking: Fields accepting some enumerated set of values (ex. `yes`/`no`/`ask`) are checked when generating the config file. In the future more checks may be added (for example time formats).

### Notes on types

The Dhall type of the config should be `List SSHConfig`, where `SSHConfig` is the type given at [resources/SSHConfig.dhall](./resources/SSHConfig.dhall) + a `host` field that is either `Text` or `List Text`.

Because all the elements of the list must be of the same type, all the fields other than `host` are `Optional`. There is an empty config at [resources/EmptySSHConfig.dhall](./resources/EmptySSHConfig.dhall) that can be merged in to ensure each config is the same shape. See [the examples](./examples).

The fields themselves are named the same as they are in the `ssh_config` man page with the first letter lowercased.

### Running

* `git clone git@github.com:robertjlooby/dhall-ssh-config.git`
* `cd dhall-ssh-config`
* `stack exec dhall-to-ssh-config -- < path/to/sshConfig.dhall > path/to/ssh/config`

### Tests

* `stack test [--file-watch]`
