[![Build Status](https://travis-ci.org/bartfrenk/jira-client.svg?branch=master)](https://travis-ci.org/bartfrenk/jira-client)

# JIRA client

JIRA command line client written in Haskell.

## Installing

Requires a working installation of Haskell Stack. To compile:

    stack build

To install the `jira-cli` executable in the local directory tree (by default in
`~/.local/bin`):

    stack install

Then fill in `res/_config.yaml` and copy to `~/.config.yaml`.

For bash completion, run:

    eval "$(jira-cli --bash-completion-script jira-cli)"

## Running

To get a list of commands, run:

    jira-cli --help
