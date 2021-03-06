#!/bin/bash

# food
if [ -f ~/.bash_ssh_agent ]; then . ~/.bash_ssh_agent; fi

getmakej() {
    echo $(($(nproc) + 1));
}

export EDITOR='vim'

alias makej="make -j$(getmakej)"
alias tmux="TERM=screen-256color-bce tmux"

alias tm="tmux"
alias tmn="tmux new-session -s"

alias sshno="ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null"
alias sshbatch="ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null -oConnectTimeout=5 -oBatchMode=yes -n"

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

wttr()
{
    # change Paris to your default location
    curl -H "Accept-Language: ${LANG%_*}" wttr.in/"${1:-11211?2}"
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

# xampp
alias lampp="sudo /opt/lampp/lampp"
if [ -d "$lampp" ]
then
  export LAMPP_HOME="/opt/lampp/lampp"
fi

alias c='clear'
alias bc='bc -l'
alias lh='ls --hide="*.pyc"'
alias l='ls'
alias lsat='ls -at'

hash nvim 2>&1 1>/dev/null && alias vim=nvim
alias vi=vim
alias v=vim
alias svi='sudo vi'
alias vis='vim "+set si"'
alias edit='vim'

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

if git --version | grep 2.17.1 2>&1 1>/dev/null; then
    if [ -f ~/src/etc/git-completion217.bash ]
    then
        . ~/src/etc/git-completion217.bash
        alias g="git"
        __git_complete g _git
    fi
else
    if [ -f ~/src/etc/git-completion.bash ]
    then
        . ~/src/etc/git-completion.bash
        alias g="git"
        __git_complete g _git
    fi
fi

# confirmation #
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'

alias clipboard='xclip -se c'

alias upgrade='sudo apt update && sudo apt upgrade -y'
alias sag='sudo apt-get'

alias agi='ag -i --ignore "*.min.js" --ignore "web/assets/fonts/" --ignore "web/assets/js/vendor/" --ignore "web_portfolio/assets/js/jquery*.js"'

## Functions

simp() {
    local port=8181
    local addr='0.0.0.0'

    if python3 --version &> /dev/null
    then
        echo "Python 3"
        python3 -m http.server --bind "$addr" "$port"
    else
        echo "Python 2"
        python -m SimpleHTTPServer "$port"
    fi
}

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
        && echo 'No search param' && return 0

    local q="$1"

    shift

    local dir="$1"

    [[ -z "$dir" ]] && dir="."

    find "$dir" -iname "*$q*"
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
        echo "\$1 hostname, \$2 host port, \$3 local port"
        return 1
    fi
    ssh -v "$1" -L "$3:$1:$2" -N
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
    < /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-32};echo;
}

wgetfullsite() {
    wget --recursive --no-clobber --page-requisites --adjust-extension --span-hosts --convert-links --no-parent "$1"
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

alias digit="dig +noall +answer"
alias diggoog="dig +noall +answer @8.8.8.8"
alias digiac="dig +noall +answer @192.168.200.29"

alias gpglist="gpg --list-secret-keys --keyid-format LONG"
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
    rawdog -N -u -w
    xdg-open ~/.rawdog/output.html
}
