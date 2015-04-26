# Utility functions to source

_does_line_exist_in_file() {
    local search="$1"
    local file="$2"

    [ $( grep -ic "$search" "$file" ) -ge 1 ] && return 0

    return 1
}
