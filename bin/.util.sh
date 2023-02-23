# Utils for scripts to be sourced

echoerr() {
	echo "$@" 1>&2
}

log() {
	fmt="$1"
	shift
	TZ='utc' printf "%(%FT%TZ)T ${fmt} \\n" -1 "$@"
}

cmd_exists() {
	[ -x "$(command -v "$1")" ] || declare -F "$1" &>/dev/null
}

yn_confirm() {
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
