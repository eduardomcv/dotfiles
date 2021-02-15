#!/bin/bash

set -euo pipefail

echo Setting up prompt...

echo "# setup git prompt" >> ~/.bashrc
echo "source /usr/lib/git-core/git-sh-prompt" >> ~/.bashrc
echo "export GIT_PS1_SHOWDIRTYSTATE=1" >> ~/.bashrc
echo "export PS1=\"\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[01;33m\]\[\033[00m\]:\[\033[01;34m\]\w\[\033[01;33m\]\$(__git_ps1 ' (%s)')\[\033[00m\]\$ \"" >> ~/.bashrc

echo Done.
