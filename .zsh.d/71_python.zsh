export PROJECT_HOME=~/devel
export VIRTUAL_ENV_DISABLE_PROMPT=1
export NODE_VIRTUAL_ENV_DISABLE_PROMPT=1

clean_pyc() {
    local dir=${1:-$PWD}
    find "$dir" \( -name __pycache__ -o -name .mypy_cache \) -type d \
         -prune -exec rm -rf {} +
    find "$dir" -regex '.+\.py[co]$' -type f -delete
}

_get_venv_prompt_info() {
    if [[ -n $VIRTUAL_ENV ]]; then
        REPLY=${VIRTUAL_ENV:t}
    fi
}

add_prompt_hook _get_venv_prompt_info
