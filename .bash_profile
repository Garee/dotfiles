#!/bin/bash

source /usr/local/bin/virtualenvwrapper.sh

export TODOIST_HOME=$HOME/dev/todoist

alias grep='grep --color=auto'
alias ls='ls -lFho'
alias tree='tree -C'
alias less='less -r'
alias vi='vim'
alias e='emacs'
alias reload='source $HOME/.bashrc'

# Get the current branch in the git repository.
function parse_git_branch() {
    BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
    if [ ! "${BRANCH}" == "" ]; then
        echo "(${BRANCH})"
    fi
}

export PS1="\n[\d \A] [\u@\h] \w \`parse_git_branch\` \n$ "
