set -e
set -o pipefail

dotfiles() {
  rsync --quiet \
    --exclude ".git/" \
    --exclude "install" \
    --exclude "README.md" \
    --exclude ".bashrc" \
    -avh --no-perms . ~

  cat .bashrc >> ~/.bashrc
}

tools() {
  apt update || true
  apt upgrade -y
  
  apt install -y \
    curl
  
  # Node Version Manager
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
}

main() {
  local cmd=$1

  if [[ $cmd == "dotfiles" ]]; then
    dotfiles
  elif [[ $cmd == "tools" ]]; then
    tools
  fi
}

main "$@"
