# .bashrc

# User specific aliases and functions

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Source Facebook definitions
if [ -f /home/engshare/admin/scripts/master.bashrc ]; then
	. /home/engshare/admin/scripts/master.bashrc
fi

if [ $TERM="dumb" ]; then
  PROMPT_COMMAND=""
fi

export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it

# Save and reload the history after each command finishes
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

alias sc="tmux attach"
alias sb="source ~/.bashrc"
alias screen="echo $DISPLAY > ~/.display; TERM=xterm screen"
alias ec="emacsclient"
alias amend="hg commit --amend"
alias st="git st"
alias sl=ls
alias grc="git rebase --continue"
alias dotfiles="cd ~/.dotfiles"
export GREP_OPTIONS='--color=auto'
export EDITOR=emacsclient

if [ -f ~/.local.bashrc ]; then
    . ~/.local.bashrc
fi


PS1="[\u \w] "