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
	command -v "$1" >/dev/null 2>&1
	return $?
}
