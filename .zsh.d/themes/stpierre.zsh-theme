# Two-line prompt built from hooks registered with add_prompt_hook (see
# ~/.zsh.d/05_prompt.zsh). Registered last, so every other precmd hook
# (e.g., the git state refresh) has already run.

setopt prompt_subst

PROMPT='${_prompt_left}'
RPROMPT='${_prompt_right}'

add-zsh-hook precmd _prompt_build
