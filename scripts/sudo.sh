check_is_sudo() {
  if [ "$(id -u)" -ne 0 ]; then
    echo "Please run as root."
    exit 1
  fi
}

check_is_not_sudo() {
  if [ "$(id -u)" -eq 0 ]; then
    echo "Please run as non-root."
    exit 1
  fi
}
