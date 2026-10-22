#!/usr/bin/env bash
set -Eeuo pipefail

# The dotfiles repo is expected at ~/.dotfiles. It is cloned manually after
# `gh auth login`, since cloning requires auth:
#   git clone <this repo> ~/.dotfiles

if [[ ! -f /etc/os-release ]] || ! grep -q '^ID=ubuntu$' /etc/os-release; then
    echo "Error: this script is intended for Ubuntu." >&2
    exit 1
fi

if ! command -v sudo >/dev/null 2>&1; then
    echo "Error: sudo is required." >&2
    exit 1
fi

echo "Updating package lists..."
sudo DEBIAN_FRONTEND=noninteractive apt update
sudo DEBIAN_FRONTEND=noninteractive apt upgrade -y

echo "Installing system packages..."
sudo DEBIAN_FRONTEND=noninteractive apt install -y \
    git \
    gh \
    emacs \
    vim \
    gcc \
    npm \
    curl \
    zsh \
    golang-go \
    make \
    bat \

sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
    build-essential \
    zlib1g-dev \
    libffi-dev \
    libssl-dev \
    libbz2-dev \
    libreadline-dev \
    libsqlite3-dev \
    liblzma-dev \
    libncurses-dev \
    tk-dev

echo "Installing LSP..."
sudo npm install --global pyright
go install golang.org/x/tools/gopls@latest

echo "Installing pyenv..."
if [[ ! -x "$HOME/.pyenv/bin/pyenv" ]]; then
    curl -fsSL https://pyenv.run | bash
else
    echo "pyenv already installed."
fi

echo "Installing oh-my-zsh..."
if [[ ! -f "$HOME/.oh-my-zsh/oh-my-zsh.sh" ]]; then
    RUNZSH=no CHSH=no KEEP_ZSHRC=yes \
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
else
    echo "oh-my-zsh already installed."
fi

echo "Installing oh-my-zsh plugins..."
PLUGINS_DIR="$HOME/.oh-my-zsh/custom/plugins"
mkdir -p "$PLUGINS_DIR"

install_plugin() {
    local url="$1" name="$2"
    local dest="$PLUGINS_DIR/$name"
    rm -rf "$dest"
    git clone --depth 1 "$url" "$dest"
}

install_plugin https://github.com/MichaelAquilina/zsh-you-should-use.git you-should-use
install_plugin https://github.com/fdellwing/zsh-bat.git zsh-bat

# Point zsh and Emacs at the dotfiles repo, which is cloned manually to ~/.dotfiles.
echo "Configuring dotfiles..."
rm -f "$HOME/.zshrc" "$HOME/.emacs"
printf 'source ~/.dotfiles/.zshrc\n' > "$HOME/.zshrc"
printf '(load-file "~/.dotfiles/.emacs")\n' > "$HOME/.emacs"

# oh-my-zsh only loads themes from its own custom dir, so link ours there.
mkdir -p "$HOME/.oh-my-zsh/custom/themes"
ln -sfn "$HOME/.dotfiles/.oh-my-zsh/custom/themes/rodp63.zsh-theme" \
        "$HOME/.oh-my-zsh/custom/themes/rodp63.zsh-theme"

echo "Setting zsh as default shell..."
sudo chsh -s "$(command -v zsh)" "$USER"

echo "Ubuntu development environment setup complete."
echo "Next: clone the dotfiles repo to ~/.dotfiles, then run 'exec zsh'."
