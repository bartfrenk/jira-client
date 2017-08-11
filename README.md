[![Build Status](https://travis-ci.org/bartfrenk/jira-client.svg?branch=master)](https://travis-ci.org/bartfrenk/jira-client)

Deprecated in favor of [time-tracker](https://github.com/bartfrenk/time-tracker).

# JIRA client

JIRA command line time tracker written in Haskell.

## Installing

Requires a working installation of Haskell Stack. To compile:

    stack build

To install the `jira-cli` executable in the local directory tree (by default in
`~/.local/bin`):

    stack install

Then fill in `res/_config.yaml` and copy to `~/.jira-cli/config.yaml`.

## Running

To get a list of commands, run:

    jira-cli --help

The normal workflow is to run:

    jira-cli start <issue number, key, or alias>

when starting whatever issue, and:

    jira-cli stop

when you go home. These commands do not modify the work logs on JIRA. To do so,
run:

    jira-cli book

This command also clears the local log. Viewing the local log is done by:

    jira-cli review

## Miscellaneous

For bash completion, run:

    eval "$(jira-cli --bash-completion-script jira-cli)"

This only enables bash completion for the fixed commands, not the aliases.
