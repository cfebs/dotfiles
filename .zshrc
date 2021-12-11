autoload -Uz compinit promptinit edit-command-line select-word-style
select-word-style bash
compinit
promptinit
setopt interactivecomments

bindkey -e

# ctrlx ctrle for editing lines
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# private stuff, no SCM
[ -f $HOME/.exports ] && source $HOME/.exports

# public common stuff between shells, tracked in SCM
[ -f $HOME/.exports.common ] && source $HOME/.exports.common

__git_files () {
    _wanted files expl 'local files' _files
}

# janky way to speed up big .ssh config completion
# @see /usr/share/zsh/functions/Completion/Unix/_ssh_hosts
_ssh_hosts() {
    host_list=($(grep -r 'Host ' ~/.ssh/*config* | awk '{s = s $2 " "} END {print s}'))
    _wanted hosts expl 'remote host name' \
      compadd -M 'm:{a-zA-Z}={A-Za-z} r:|.=* r:|=*' "$@" $host_list
}

# function refresh_ssh_autocomplete () {
#     host_list=($(grep -r 'Host ' ~/.ssh/ | awk '{s = s $2 " "} END {print s}'))
#     zstyle ':completion:*:(scp|rsync):*' tag-order ' hosts:-ipaddr:ip\ address hosts:-host:host files'
#     zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
#     zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'
#     zstyle ':completion:*:(ssh|scp|sftp):*' hosts $host_list
# }
# refresh_ssh_autocomplete
# zstyle ':completion:*' hosts off


hash starship 1>/dev/null 2>&1 && eval "$(starship init zsh)"
