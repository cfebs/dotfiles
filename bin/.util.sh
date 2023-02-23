# Utils for scripts to be sourced

utl_echoerr() {
	echo "$@" 1>&2
}

utl_log() {
	# Usage:
	# utl_log 'one simple arg'
	# utl_log '%s %d' 'more args' 50

	fmt="$1"
	shift
	TZ='utc' printf "%(%FT%TZ)T ${fmt} \\n" -1 "$@" 1>&2
}

utl_cmd_exists() {
	[ -x "$(command -v "$1")" ] || declare -F "$1" &>/dev/null
}

utl_yn_confirm() {
	read -r -p "Are you sure? [y/N] " response
	case "$response" in
		[yY][eE][sS]|[yY])
			return 0
			;;
		*)
			return 1
			;;
	esac
	return 1
}
