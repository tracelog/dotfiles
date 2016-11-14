#
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#

autoload -U compinit
compinit

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

## keep background processes at full speed
#setopt NOBGNICE
## restart running processes on exit
#setopt HUP

## history
export HISTFILE=~/.zsh_history
export SAVEHIST=65535
export HISTSIZE=65535
setopt SHARE_HISTORY

## never ever beep ever
#setopt NO_BEEP

## automatically decide when to page a list of completions
#LISTMAX=0

## disable mail checking
#MAILCHECK=0

autoload -U colors
colors

PS1="[%n %~] "


setopt autocd
setopt extendedglob


alias amend="hg commit --amend"
alias dotfiles="cd ~/.dotfiles"
alias ec="emacsclient"
alias grc="git rebase --continue"
alias ls='ls --color=auto'
alias sb="source ~/.zshrc"
alias sc="tmux attach"
alias screen="echo $DISPLAY > ~/.display; TERM=xterm screen"
alias sl=ls
alias st="git st"
export EDITOR=emacsclient
export GREP_OPTIONS='--color=auto'



if [ -f ~/.local.zshrc ]; then
    source ~/.local.zshrc
fi
