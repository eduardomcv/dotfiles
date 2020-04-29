# git branch prompt
source /usr/share/git/completion/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export PS1='\[\033[1;32m\][\u@\h \[\033[1;34m\]\w\[\033[1;32m\]]\[\033[01;33m\]$(__git_ps1)\[\033[01;34m\]\[\033[1;32m\]\$\[\033[00m\] '

# git bash completion
source /usr/share/git/completion/git-completion.bash

