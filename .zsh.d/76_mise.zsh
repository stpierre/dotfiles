# the oh-my-zsh mise plugin runs `mise activate`

if (( $+commands[mise] )); then
    alias m=mise
    alias mr="mise run"
fi
