set -e
set -o pipefail

rsync --quiet \
    --exclude ".git/" \
    --exclude "install" \
    --exclude "README.md" \
     -avh --no-perms . ~

