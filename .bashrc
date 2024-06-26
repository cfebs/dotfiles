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
append_path() {
	case ":$PATH:" in
		*:"$1":*)
			# debug
			# echo ">> append $1 is already in path" 1>&2
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
			# echo ">> prepend $1 is already in path" 1>&2
			;;
		*)
			PATH="$1${PATH:+:$PATH}"
	esac
}

# Append "$1" to $PROMPT_COMMAND when not already in.
append_promptcmd() {
	case ";$PROMPT_COMMAND;" in
		*";$1;"*)
			# debug
			# echo ">> append $1 is already in prompt_command" 1>&2
			;;
		*)
			PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}$1"
	esac
}

# Prepend "$1" to $PROMPT_COMMAND when not already in.
prepend_promptcmd() {
	case ";$PROMPT_COMMAND;" in
		*";$1;"*)
			# debug
			# echo ">> prepend $1 is already in prompt_command" 1>&2
			;;
		*)
			PROMPT_COMMAND="$1${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
			;;
	esac
}

# individual host stuff, no SCM
[ -f ~/.exports ] && source ~/.exports

# public common stuff between shells, tracked in SCM
[ -f ~/.exports.common ] && source ~/.exports.common

# starship after path manipulation. obey env var to disable it if needed
if [ -z "$DISABLE_STARSHIP" ] && [ -x "$(command -v "starship")" ] || declare -F "$1" &>/dev/null; then
	eval "$(starship init bash)"
else
	if [[ -f ~/.git-prompt.sh ]]; then
		source ~/.git-prompt.sh
		export GIT_PS1_SHOWDIRTYSTATE=1
		export GIT_PS1_SHOWUNTRACKEDFILES=1
		PS1='[\u@\h \W]$(__git_ps1 " (%s)")\n❯ '
	fi
fi


# more private/on-demand env variables
if [ ! -e ~/.exports.creds ]; then
	touch ~/.exports.creds && chmod 700 ~/.exports.creds
fi

[ -f ~/Sync/exports ] && source ~/Sync/exports

unset -f append_path prepend_path append_promptcmd prepend_promptcmd
