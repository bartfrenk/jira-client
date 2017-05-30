# JIRA client

JIRA command line client written in Haskell.

## Installing

Requires a working installation of Haskell Stack. To compile:

    stack build

To install the `jira-cli` executable in the local directory tree (by default in
`~/.local/bin`):

    stack install

Then fill in `res/_jira-cli.yaml` and copy to `~/.jira-cli.yaml`.

For bash completion, run:

    eval "$(jira-cli --bash-completion-script jira-cli)"

## Running

Supports command `log` and `search`. To get help on a command run:

    jira-cli <command> --help

