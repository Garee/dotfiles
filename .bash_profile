#!/bin/bash

# To allow use of the virtualenvwrapper script.
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi

# Put global npm packages on our path.
if [ -d /Users/gary/.npm/bin ]; then
    export PATH=$PATH:'/Users/gary/.npm/bin'
fi

# Aliases
alias grep='grep --color=auto'
alias ls='ls -lFho'
alias vi='vim'
alias e='emacs'
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
