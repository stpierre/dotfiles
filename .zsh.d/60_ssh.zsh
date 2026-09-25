# https://developer.1password.com/docs/ssh/get-started#step-4-configure-your-ssh-or-git-client
# this is just for mac os; would be nice to have it for linux too
_OP_AGENT_SOCK="$HOME/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"

if [[ -e $_OP_AGENT_SOCK ]]; then
    export SSH_AUTH_SOCK=$_OP_AGENT_SOCK
else
    SSH_AGENT_FILE=${${:-$HOME/.ssh-agent-info}:A}

    start-ssh-agent() {
        local proc cmds
        if (( ! $+commands[ssh-agent] )); then
            return 0
        fi
        if [[ -e $SSH_AGENT_FILE ]]; then
            source "$SSH_AGENT_FILE"
            proc=$(ps -p "$SSH_AGENT_PID" -o comm= 2>/dev/null)
            if [[ ${proc:t} == ssh-agent ]]; then
                return 0
            fi
            unset SSH_AUTH_SOCK SSH_AGENT_PID
            rm -f "$SSH_AGENT_FILE"
        fi
        cmds=$(ssh-agent | grep -v '^echo') || return
        eval "$cmds" || return
        print -r -- "$cmds" >| "$SSH_AGENT_FILE"
    }

    start-ssh-agent
fi
