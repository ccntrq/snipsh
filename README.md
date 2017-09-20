# snipsh - A convenient CLI util for the GitLab snippet API

This is a CLI utility to conveniently view and execute your GitLab snippets.

Notable features so for are:

- list your personal snippet index
- view a single snippet
- execute a snippet. This will open the content of the snippet in your editor
  and execute the modified content after saving

# Config

You need to supply a little configuration details for

- your GitLab access token
- the url to your GitLab instance
- and your favorite editor

There is a default config located at `config.default.json` that you can copy to
`config.json` and modify to your needs

# Build

This project is written in Haskell and build using cabal. There is a cabalfile
in the repository that can be used to install the project and its dependencies.

You can install and build in a sandbox like so

```
# initial steps. only needed on first run
cabal sandbox init         # initialize the sandbox
cabal sandbox add-source . # add source to the sandbox
cabal install              # fetch and install the dependencies into the sandbox
cabal build                # build
# run!
dist/build/snipsh/snipsh --help
```

You should now have a runnable binary located at 'dist/build/snipsh'
