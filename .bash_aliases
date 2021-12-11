#!/bin/bash

getmakej() {
    echo $(($(nproc) + 1));
}

alias tailcat="tail -n +1"
alias dok="docker"
alias dokc="docker-compose"
alias makej="make -j$(getmakej)"
alias tmux="TERM=screen-256color-bce tmux"
alias rgi="rg -i"

alias tm="tmux"
alias tmn="tmux new-session -s"

alias sshno="ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null"
alias sshbatch="ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null -oConnectTimeout=5 -oBatchMode=yes -n"

alias nethack-remote="alacritty --config-file ~/.config/alacritty/alacritty-dark.yml -e /bin/bash -c 'ssh nethack@alt.org'"
alias alacritty-dark="alacritty --config-file ~/.config/alacritty/alacritty-dark.yml"

alias gistdiff="gist -f diff.diff"

alias somafm="~/.yarn/bin/somafm"
alias somafm-groove="~/.yarn/bin/somafm play groovesalad"
alias somafm-drone="~/.yarn/bin/somafm play dronezone"

alias yarn-linked="find . -type l | grep -v .bin | sed 's/^\.\/node_modules\///'"
alias yarn-unlink-all="yarn-linked | xargs yarn unlink && yarn install --check-files"

alias bonsai="cbonsai -li -t 0.35 -M 7"

alias ncdu-full="sudo ncdu --exclude '/media/*' --exclude '/proc/*' --exclude '/sys/*' /"

alias k9s="/bin/k9s --readonly"
alias k9sw="/bin/k9s --write"

iscmd() {
    # does the command exist
    [ -x "$(command -v "$1")" ] || declare -F "$1" &>/dev/null
}

digany() {
    dig +noall +answer "$1" ANY
}

myscrot() {
    mkdir -p ~/Pictures/Screenshots
    scrot ~/Pictures/Screenshots/%Y-%m-%d_\$wx\$h.png "$@"
}

tma() {
    tmux attach -t $(tmux ls | fzf | cut -d: -f1)
}

tmnd() {
    tmux new-session -s $(pwd)
}

sshscreen() {
    # ssh's to the box and runs screen attach
    ssh -t $1 screen -Rad
}

wttr() {
    # change Paris to your default location
    curl https://wttr.in/"${1:-lga?3}"
}

# Quick nav
alias 1="cd ../"
alias 2="cd ../../"
alias 3="cd ../../../"
alias 4="cd ../../../../"
alias ttime="/usr/bin/time"

alias bashrc=". ~/.bashrc"

grep_alias='grep'
if grep --color "a" <<<"a" &>/dev/null; then
    grep_alias="$grep_alias --color=auto"
fi
alias grep="$grep_alias"

alias less="less -R"

alias c='clear'
alias bc='bc -l'
alias lt='ls -lat | head -n20'
alias l='ls'
alias lsat='ls -at'

export EDITOR='vim'
if hash nvim 2>&1 1>/dev/null; then
    export EDITOR='nvim'
    alias vim=nvim
fi

alias vi=$EDITOR
alias v=$EDITOR
alias svi="sudo $EDITOR"
alias vis="$EDITOR \"+set si\""
alias edit="$EDITOR"

alias dmenu="dmenu -b -i -l 15 -fn '-misc-fixed-medium-r-normal--18-*-*-*-*-*-*-*'"
alias dmenu_run="dmenu_run -b -i -l 15 -fn '-misc-fixed-medium-r-normal--18-*-*-*-*-*-*-*'"

# -index_interval duration
#   interval of indexing; 0 for default (5m), negative to only index once at startup
alias localgodoc="godoc -http=:6060 -index -play -index_interval -1m"

alias rm='rm'
# do not delete / or prompt if deleting more than 3 files at a time #
TEST_FILE="$HOME/.rm_preserve_test" && touch $TEST_FILE && rm --preserve-root $TEST_FILE 2> /dev/null && \
    alias rm='rm --preserve-root'

TEST_FILE="$HOME/.rm_preserve_test" && touch $TEST_FILE && rm -I $TEST_FILE 2> /dev/null && \
    alias rm='rm -I'

alias g="git"
if [ -z "$ZSH_VERSION" ] && [ -f ~/.git-completion.sh ]; then
    source ~/.git-completion.sh
    __git_complete g _git
fi

# confirmation #
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'

alias clipboard='xclip -se c'

# replaced by agignore stuff
#alias agi='ag -i --ignore "*.min.js" --ignore "web/assets/fonts/" --ignore "web/assets/js/vendor/" --ignore "web_portfolio/assets/js/jquery*.js"'

## Functions

#simp() {
#    local port=${1-8181}
#    local addr=${2-0.0.0.0}
#
#    if python3 --version &> /dev/null
#    then
#        echo "Python 3"
#        python3 -m http.server --bind "$addr" "$port"
#    else
#        echo "Python 2"
#        python -m SimpleHTTPServer "$port"
#    fi
#}

pn() {
    while read data;
    do
        echo $data | awk "{ print \$$1 }"
        #echo $data | cut -d' ' -f$1
    done
}

g_rlc() {
    git checkout $(git reflog | grep checkout | head -80 | grep moving | pn 8 | grep -v '^master$' | fzf)
}

rvm-project() {
  if [ -z "$1" ]
  then
    echo Need name argument
  else
    mkdir "$1"
    rvm gemset create "$1"
    echo "rvm gemset use $1" > ./$1/.rvmrc
    echo '.rvmrc' >> ./$1/.gitignore
  fi
}

function timer_notify() {
    echo -e "\n\nBOOOM! Time to start."
    notify-send -u critical "`echo BOOOOOOM | figlet`"
}

function timer() {
    MIN=$1 && for i in $(seq $(($MIN*60)) -1 1); do echo -n "$i, "; sleep 1; done; timer_notify;
}

alias ack="ack-grep"
function qfind() {

    [[ -z $1 ]] \
        && echo -e "No search param\n\nUsage: ${FUNCNAME[0]} <query> [dir]" && return 0

    local q="$1"
    shift

    local dir="$1"
    shift

    [[ -z "$dir" ]] && dir="."

    find "$dir" "$@" -iname "*$q*"
}

function qgrep() {
    grep -irn "$1" .
}

function net-qual() {
    # 1st arg is the domain to make a HEAD request to
    while true; do echo 'start'; time curl --head "$1"; echo 'finish & sleeping'; sleep 10; done;
}

alias htmlencode="perl -MHTML::Entities -pe 'encode_entities(\$_)'"
alias htmldecode="perl -MHTML::Entities -pe 'decode_entities(\$_)'"

__my_journal() {
    local journal_dir="$HOME/quick_journal"
    local journal_file="$(date +%Y_%m_%d).md"

    if [ ! -d "$journal_dir" ]
    then
        mkdir "$journal_dir";
        echo "Creating $journal_dir"
    fi


    if [ ! -f "$journal_dir/$journal_file" ]
    then
        echo "Creating todays file $journal_file"
        touch "$journal_dir/$journal_file"
    fi

    local content="- \`$(date +%H:%M:%S)\` $@"
    echo "$content" >> "$journal_dir/$journal_file"
    echo "Wrote -> $content to $journal_file"

    return 0;
}

alias j="__my_journal"

dodat() {
    # dodat cmd src_file relative_target
    # dodat cp /tmp/dir/file.txt new_file.txt
    # => cp /tmp/dir/file.txt /tmp/dir/new_file.txt
    # dodat mv /tmp/dir/file.txt new_file.txt
    # => mv /tmp/dir/file.txt /tmp/dir/new_file.txt
    # dodat touch /tmp/dir/file.txt new_file.txt
    # => touch /tmp/dir/new_file.txt
    local cmd="$1"
    local src="$2"
    local target="$3"
    local dest_dir="$(dirname "$src")"
    local result="${dest_dir}/${target}"

    case "$cmd" in
        "cp" | "mv")
            # targets
            $cmd "$src" "$result";
            echo "$cmd $src $result";
            ;;
        "touch")
            $cmd "$result";
            echo "$cmd $result";
            ;;
        *)
            echo "$cmd not supported"
            ;;
    esac
}

mkcp() {
    local src="$1"
    local dest="$2"
    mkdir -pv "$src"
    cp "$src" "$dest"
}

npmbin() {
    $(npm bin)/"$@"
}

# http://www.commandlinefu.com/commands/view/9807/convert-number-of-bytes-to-human-readable-filesize
human_filesize() {
    awk -v sum="$1" ' BEGIN {hum[1024^3]="Gb"; hum[1024^2]="Mb"; hum[1024]="Kb"; for (x=1024^3; x>=1024; x/=1024) { if (sum>=x) { printf "%.2f %s\n",sum/x,hum[x]; break; } } if (sum<1024) print "1kb"; } '
}

pphtml() {
    url="$1"
    shift
    curl -s "$url" | pup --color "$@"
}

human_size() {
pyscript=$(cat <<SCRIPT
def sizeof_fmt(num, suffix='B'):
    for unit in ['','Ki','Mi','Gi','Ti','Pi','Ei','Zi']:
        if abs(num) < 1024.0:
            return "%3.1f%s%s" % (num, unit, suffix)
        num /= 1024.0
    return "%.1f%s%s" % (num, 'Yi', suffix)
print sizeof_fmt($1)
SCRIPT
)
    python -c "$pyscript"
}

set_title()
{
    ORIG=$PS1
    TITLE="\e]2;$*\a"
    PS1=${ORIG}${TITLE}
}

pssh() {
    # $1 some string to eval 'pweb{1..10}'
    # $2 cmd
    # Usage: pssh 'pweb{1..10}' 'hostname'
    echo "Running on $(eval echo $1)..."
    sleep 0.7
    parallel-ssh -O StrictHostKeyChecking=no -i -H "$(eval echo $1)" $2
}

alias sshvpn="ssh -c 3des-cbc"

knownhostsrm() {
    ssh-keygen -f $HOME/.ssh/known_hosts -R $1
    for ip in $(dig +short $1)
    do
        ssh-keygen -f $HOME/.ssh/known_hosts -R $ip
    done
}

sshtun() {
    # $1 hostname
    # $2 host port
    # $3 local port
    if [ $# -lt 3 ]
    then

        echo "Usage: ${FUNCNAME[0]} <connect-to-addr> <remote-port> <local-port> [remote-addr]"
        echo ""
        echo "Arguments:"
        echo "  connect-to-addr:  ssh will connect to this address or hostname"
        echo "  remote-port:      forward traffic from this port on the destination to the local port"
        echo "  local-port:       local port to forward to"
        echo "  remote-addr:      default: localhost, connect to this address on the remote"
        echo "Examples:"
        echo ""
        echo "  # connect to no.city, make no.city:8545 (localhost) -> localhost:9545"
        echo "  ${FUNCNAME[0]} no.city 8545 9545"
        return 1
    fi

    local connectto="${1}"
    local remoteport="${2}"
    local localport="${3}"
    local remoteaddr="${4-localhost}"

    echo "connect-to-addr: $connectto, remoteport: $remoteport, local-port: $localport, remote-addr: $remoteaddr"

    ssh -v -N "$connectto" -L "$localport:$remoteaddr:$remoteport" -N
}

socksproxy() {
    if [ $# -lt 2 ]
    then
        echo "Usage: socksproxy localport user@host [sshport]"
        echo "    \$1: localport"
        echo "    \$2: login"
        echo "    \$3: sshport (default 22)"
        return 1
    fi

    sshport=${3-22}

    echo "localport: $1, auth: $2, sshport: $sshport"
    ssh -v -N -p "$sshport" -D "0.0.0.0:$1" "$2"
}

lookupip() {
    curl "https://tools.keycdn.com/geo.json?host=$1" | jq .
}

helpme() {
    export LESS=-RXFi
    export LESS_TERMCAP_mb=$(printf "\e[1;31m")
    export LESS_TERMCAP_md=$(printf "\e[1;31m")
    export LESS_TERMCAP_me=$(printf "\e[0m")
    export LESS_TERMCAP_se=$(printf "\e[0m")
    export LESS_TERMCAP_so=$(printf "\e[1;44;33m")
    export LESS_TERMCAP_ue=$(printf "\e[0m")
    export LESS_TERMCAP_us=$(printf "\e[1;32m")

    tldr "$@" && env man "$@"
}

alias randpass="cat /dev/urandom | head -c 15 | base64"
alias proxydrop="socksproxy 9999 collin@proxydrop 222"

rubycheck() {
    echo "Checking .rb"
    find . -type f -name '*.rb' -exec ruby -c {} \; > /dev/null
    echo "Checking .erb"
    find . -type f -name '*.erb' -exec sh -c "erb -x -T '-' {} | ruby -c" \; > /dev/null
    echo "Done"
}

genpassword() {
    if iscmd pwgen; then
        pwgen -scy 32 | head -n1
        return $?
    fi

    < /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-32};echo;
}

wgetfullsite() {
    local ua="Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.182 Safari/537.36"
    wget -r -np -p -E -k -K --random-wait -e robots=off --user-agent "$ua" "$@"
}

alias docker_clean_ps='docker rm $(docker ps --filter=status=exited --filter=status=created -q)'
alias remoteirc="mosh no.city -- screen -rad weechat"

aws-mfa-vimeo() {
    eval $(AWS_MFA_ARN="$AWS_MFA_ARN" AWS_PROFILE=vimeo $HOME/src/vimeo/ops/aws/aws-mfa.py)
}

aws-mfa-vhx-legacy() {
    eval $(AWS_MFA_ARN="$AWS_VHX_LEGACY_MFA_ARN" AWS_PROFILE=vhx-legacy $HOME/src/vimeo/ops/aws/aws-mfa.py)
}

alias qfeh="feh -C /usr/share/fonts/truetype/dejavu -e DejaVuSans/8 -. -Z -B black -g800x600"
aws-mfa-pci() {
    eval $(AWS_MFA_ARN="$AWS_PCI_MFA_ARN" AWS_PROFILE=pci $HOME/src/vimeo/ops/aws/aws-mfa.py)
}

alias qdig="dig +noall +answer"
alias diggoog="dig +noall +answer @8.8.8.8"
alias digiac="dig +noall +answer @192.168.200.29"

alias gpgdecrypt="gpg -d"
# gpgimport ./priv.asc
alias gpgimport="gpg --import"
alias gpglist="gpg --list-secret-keys --keyid-format LONG"
alias gpglistpub="gpg --list-keys --keyid-format LONG"
gpgencrypt() {
    # use gpglistpub to list recips
    recip="$1"
    gpg -r "$recip" --encrypt --armor
}
gpgexport() {
    id="$1"
    echo "Using key id $id, listing key"
    gpg --list-keys "$id"
    rc=$?
    [ $rc != 0 ] && echo "Bad id" && return 1

    echo "Look ok? Ctrl-C to exit, enter to continue"
    read a

    dir=$(mktemp -d)
    echo "Create temp dir $dir, moving there"
    sleep 1
    cd $dir
    echo "Exporting"
    gpg --armor --export "$id" > pub.asc
    gpg --armor --export-secret-keys "$id" > priv.asc
    gpg --export-ownertrust trust > trust

    echo "Done"
    ls -lat .
}

certinfo() {
    if [ -z "$1" ]; then
        echo "Usage: ${FUNCNAME[0]} <domain>"
        return 1
    fi
    echo "QUIT" | timeout 3 openssl s_client -connect "$1:443" 2>&1 | openssl x509 -noout -text 2>&1
}

jqdiff() {
    if [[ ! -e "$1" ]] || [[ ! -e "$2" ]]; then
        echo "Error: a provided file does not exist"
        echo "Usage: ${FUNCNAME[0]} <file1> <file2>"
        return 1
    fi
    diff <(jq -S . "$1") <(jq -S . "$2")
}

flattenlink() {
    full_path="$1"
    if [ -z "$full_path" ]; then
        "Usage: $0 <absolute path to link>"
        return 1
    fi

    no_slash="${full_path#\/}"
    if [ "$no_slash" == "$full_path" ]; then
        echo "Please provide path from root"
        return 1
    fi

    real_path="$(readlink -f "$full_path")"

    if [ "$real_path" == "$full_path" ]; then
        echo "File paths are the same, nothing to do"
        return 0
    fi

    echo "Unlinking $full_path, Copying $real_path to $full_path"
    unlink "$full_path"
    cp "$real_path" "$full_path"
    echo "Done"
}

rss-read() {
    rawdog -Nuw
    xdg-open ~/.rawdog/output.html
}

update-pacman-mirrors() {
    echo "Starting reflector update" 1>&2
    sudo reflector --connection-timeout 2 -c 'United States' -a 3 -p https -i '\.edu' \
        -f 5 --save /etc/pacman.d/mirrorlist
    rc=$?
    cat /etc/pacman.d/mirrorlist 1>&2
    echo "Done. Exit $rc" 1>&2
}

vimeo-vpn() {
    nmcli c up Vimeo_collin@vimeo.com_us-udp
}

tunnel-syncthing() {
    sshtun droplet2 8384 8385
}

percent-change() {
python3 -c 'import sys; z, a, b = sys.argv; r=(float(b) - float(a)) / float(a); print(round(r*100.0, 4),"%change")' $@
}

docxtotxt() {
    abiword --to=txt --to-name=fd://1 "$1"
}

bak() {
    local target="$1"
    if [[ ! -e "$target" ]]; then
        echo ">> Target for bak $target does not exist" 1>&2
        return 1
    fi

    local abs="$(readlink -f "$target")"
    local bak_dest="${abs}.bak"

    echo ">> Creating $bak_dest from $abs" 1>&2
    cp "$abs" "$bak_dest"
    echo "Done" 1>&2
}

unbak() {
    local target="$1"
    if [[ ! -e "$target" ]]; then
        echo ">> Target for unbak $target does not exist" 1>&2
        return 1
    fi

    local abs="$(readlink -f "$target")"
    local unbaked="$(echo "$abs" | sed 's/\.bak$//g')"

    if [[ "$abs" == "$unbaked" ]]; then
        echo ">> Target for unbak $target does not contain .bak" 1>&2
        return 1
    fi

    # copy existing unbaked to .bak.bak
    dbl_bak="${unbaked}.bak.bak"
    echo ">> Copying $unbaked to $dbl_bak" 1>&2
    cp "$unbaked" "$dbl_bak"

    # mv .bak to existing
    echo ">> Move $abs to $unbaked" 1>&2
    mv "$abs" "$unbaked"

    # mv .bak.bak to .bak
    echo ">> Move $dbl_bak to $abs" 1>&2
    mv "$dbl_bak" "$abs"
}

tenor() {
    args="$@"
    query="$(echo "$@" | sed 's/ \+/+/g')"
    echo "Query: $query" 1>&2
    curl -sL "https://api.tenor.com/v1/search?q=${query}&key=&limit=3" | jq -r '.results[].media[].mediumgif.url'
}

jwt-decode() {
     jq -R 'split(".") | .[0],.[1] | @base64d | fromjson' <<< "${1}"
     echo "Signature: $(echo "${1}" | awk -F'.' '{print $3}')"
}

# bt-sony-pair() {
#     scanned="$(timeout 15s bluetoothctl scan on)"
#     if ! echo "$scanned" | grep -q 'CC:98:8B:B6:B3:3B'; then
#         echo "Could not find sony WH-1000XM3 CC:98:8B:B6:B3:3B in scan. Is it in pairing mode?"
#         return 1
#     fi
#
#     bluetoothctl pair CC:98:8B:B6:B3:3B && \
#         bluetoothctl trust CC:98:8B:B6:B3:3B
# }
#
# bt-sony-on() {
#     bluetoothctl connect CC:98:8B:B6:B3:3B
# }
#
# bt-sony-off() {
#     bluetoothctl disconnect CC:98:8B:B6:B3:3B
# }


# https://github.com/jarun/nnn/blob/master/misc/quitcd/quitcd.bash_zsh
n ()
{
    # Block nesting of nnn in subshells
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

check-internet() {
    ping 8.8.8.8 | grep --line-buffered 'time=' | while read -r line; do
        echo "$line"; echo internet | speak-ng; sleep 2;
    done;
}

agr() {
    ag -0 -l "$1" | xargs -0 perl -pi -e "s/$1/$2/g"
}

agrbak() {
    ag -0 -l "$1" | xargs -0 perl -pi.bak -e "s/$1/$2/g"
}

youtube-dl-gem() {
    url="$1"
    shift 1
    output="$1"
    shift 1
    if [[ -z "$url" ]]; then echo "Missing url first arg"; return 1; fi
    output=
    ism_name="$(echo "$url" | awk -F'/' '{print $5}')"
    if [[ -n "$ism_name" ]] && echo "$ism_name" | grep -q '.ism'; then
        output="$(echo "$ism_name" | sed 's/.ism$/.mp4/g')"
        echo "Using output: $output" 1>&2
    fi
    if [[ -z "$output" ]]; then echo "Missing output format second arg"; return 1; fi
    youtube-dl -f 'bestvideo+bestaudio[format_id=audio-English]' -o "$output" "$@" "$url"
}

git-init-bare() {
    dest="~/src/${1}.git"
    ssh no.city "git init --bare $dest; cd $dest; git symbolic-ref HEAD refs/head/main"
}

datemili() {
    milis="$1"
    date -d@$((milis / 1000))
}

export CHROME_USER_AGENT="Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.45 Safari/537.36"
