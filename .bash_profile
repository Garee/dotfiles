#!/bin/bash

if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi

# Aliases
alias grep='grep --color=auto'
alias ls='ls -lFho'
alias reload='source $HOME/.bash_profile'

# Get the current branch in the git repository.
function parse_git_branch() {
    BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
    if [ ! "${BRANCH}" == "" ]; then
        echo "(${BRANCH})"
    fi
}

# Set the look of the command line prompt.
export PS1="\n[\d \A] [\u@\h] \w \`parse_git_branch\` \n$ "
