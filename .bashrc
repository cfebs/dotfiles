#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

# REMINDER this is tracked in public github
# No secrets in bashrc, put it in .exports

# Taken from /etc/profile, clever PATH manipulation
# https://github.com/archlinux/svntogit-packages/blob/6a803636e849903d1860c53c3ede64277419c4c8/trunk/profile#L6-L15
# Append "$1" to $PATH when not already in.
append_path () {
    case ":$PATH:" in
        *:"$1":*)
            # debug
            # echo ">> append $1 is already in path"
            ;;
        *)
            PATH="${PATH:+$PATH:}$1"
    esac
}

# Prepend "$1" to $PATH when not already in.
prepend_path() {
    case ":$PATH:" in
        *:"$1":*)
            # debug
            # echo ">> prepend $1 is already in path"
            ;;
        *)
            PATH="$1${PATH:+:$PATH}"
    esac
}

# private stuff, no SCM
[ -f $HOME/.exports ] && source $HOME/.exports

# public common stuff between shells, tracked in SCM
[ -f $HOME/.exports.common ] && source $HOME/.exports.common


if hash starship 1>/dev/null 2>&1; then
    eval "$(starship init bash)"
else
    [[ -f ~/.git-prompt.sh ]] && source ~/.git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWUNTRACKEDFILES=1
    PS1='[\u@\h \W]$(__git_ps1 " (%s)")\n\$ '
fi

unset -f append_path prepend_path
