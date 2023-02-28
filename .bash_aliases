#!/usr/bin/env bash
# Aliases and helper functions.
# Sourced by .exports.common (try to keep `export`s to exports. files)
#
# Tips:
# * If a function does not need to manipulate current shell session, consider a ~/bin
# * If a function is one line, consider an alias

source ~/bin/.util.sh

if utl_cmd_exists lsd; then
	alias ls='lsd'
elif ls --color /dev/null 1>/dev/null 2>&1; then
	alias ls='ls --color=auto'
fi

if grep --color "a" <<<"a" &>/dev/null; then
	alias grep="grep --color=auto"
fi

if utl_cmd_exists nvim; then
	export EDITOR='nvim'
	alias vim="$EDITOR"
	alias vi="$EDITOR"
	alias edit="$EDITOR"
fi

# ubuntu installs this as a bin called gist-paste, weird
# https://github.com/defunkt/gist
if hash gist-paste 1>/dev/null 2>&1; then
	alias gist=gist-paste
fi

#alias tmux="TERM=screen-256color-bce tmux"
alias 1="cd ../"
alias 2="cd ../../"
alias 3="cd ../../../"
alias 4="cd ../../../../"
alias alacritty-dark="alacritty --config-file ~/.config/alacritty/alacritty-dark.yml"
alias bashrc=". ~/.bashrc"
alias bc='bc -l'
alias bonsai="cbonsai -li -t 0.35 -M 7"
alias c='clear'
alias cp='cp -i'
alias curltime="curl -w %{stderr}%{time_connect}:%{time_starttransfer}:%{time_total}"
alias date-now="date +%s"
alias date-zulu="TZ='utc' printf '%(%FT%TZ)T\n'"
alias dk="docker"
alias dkc="docker compose"
alias dmenu="dmenu -b -i -l 15 -fn '-misc-fixed-medium-r-normal--18-*-*-*-*-*-*-*'"
alias dmenu_run="dmenu_run -b -i -l 15 -fn '-misc-fixed-medium-r-normal--18-*-*-*-*-*-*-*'"
alias docker-clean-ps='docker rm $(docker ps --filter=status=exited --filter=status=created -q)'
alias g="git"
alias gistdiff="gist -f diff.diff"
alias htmldecode="perl -MHTML::Entities -pe 'decode_entities(\$_)'"
alias htmlencode="perl -MHTML::Entities -pe 'encode_entities(\$_)'"
alias ixio="curl -F 'f:1=<-' ix.io"
alias j="liljournal"
alias k9sr="k9s --readonly"
alias k9sw="k9s --write"
alias l='ls'
alias ll="ls -lat"
alias ln='ln -i'
alias localgodoc="godoc -http=:6060 -index -play -index_interval -1m"
alias makej="make -j$(($(nproc) - 1))"
alias mv='mv -i'
alias ncdu-full="sudo ncdu --exclude '/media/*' --exclude '/proc/*' --exclude '/sys/*' /"
alias nethack-remote="alacritty --config-file ~/.config/alacritty/alacritty-dark.yml -e /bin/bash -c 'ssh nethack@alt.org'"
alias pacman-remove-orphans="sudo pacman -Qtdq | sudo pacman -Rns -"
alias proxydrop="socksproxy 9999 collin@proxydrop 222"
alias qfeh="feh -C /usr/share/fonts/truetype/dejavu -e DejaVuSans/8 -. -Z -B black -g800x600"
alias remoteirc="mosh no.city -- screen -rad weechat"
alias rgi="rg -i"
alias sshbatch="ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null -oConnectTimeout=5 -oBatchMode=yes -n"
alias sshno="ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null"
alias tailcat="tail -n +1"
alias tm="tmux"
alias tmn="tmux new-session -s"
alias yarn-linked="find . -type l | grep -v .bin | sed 's/^\.\/node_modules\///'"
alias yarn-unlink-all="yarn-linked | xargs yarn unlink && yarn install --check-files"

# do not delete / or prompt if deleting more than 3 files at a time #
# TODO: document where this comes from, its old...
TEST_FILE="$HOME/.rm_preserve_test" && touch $TEST_FILE && rm --preserve-root $TEST_FILE 2> /dev/null && \
	alias rm='rm --preserve-root'

TEST_FILE="$HOME/.rm_preserve_test" && touch $TEST_FILE && rm -I $TEST_FILE 2> /dev/null && \
	alias rm='rm -I'


## Functions

bins-list() {
	find ${PATH//:/" "} -executable -printf "%f\n" 2>/dev/null | sort | uniq
}

tma() {
	tmux attach -t $(tmux ls | fzf | cut -d: -f1)
}

tmnd() {
	tmux new-session -s $(pwd)
}

pn() {
	# print nth thing with default awk -F
	while read data;
	do
		echo $data | awk "{ print \$$1 }"
	done
}

grlc() {
	git checkout $(git reflog | grep checkout | head -80 | grep moving | pn 8 | grep -v '^master$' | fzf)
}

function timer_notify() {
	echo -e "\n\nBOOOM! Time to start."
	notify-send -u critical "`echo BOOOOOOM | figlet`"
}

function timer() {
	MIN=$1 && for i in $(seq $(($MIN*60)) -1 1); do echo -n "$i, "; sleep 1; done; timer_notify;
}

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

set_title() {
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

genpassword() {
	if utl_cmd_exists pwgen; then
		pwgen -scy 32 | head -n1
		return $?
	fi

	< /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-32};echo;
}

wgetfullsite() {
	local ua="Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.182 Safari/537.36"
	wget -r -np -p -E -k -K --random-wait -e robots=off --user-agent "$ua" "$@"
}

docker-compose-tail() {
	docker-compose ps "$1" 1>/dev/null 2>&1;
	rc=$?
	if [[ $rc != 0 ]];
	then
		echo "ERROR: $1 service not found"
		return 1
	fi
	echo "Tailing logs for $1" 1>&2
	docker-compose logs -f --tail=0 "$1"
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

vimeo-vpn() {
	nmcli c up Vimeo_collin@vimeo.com_us-udp
}

tunnel-syncthing() {
	ssh -L 8385:127.0.0.1:8384 collin@droplet2 -N
}

proxy-laptop() {
	# socks proxy
	# few ports for dev
	ssh -v -N \
		-D localhost:1137 tennisl1 \
		-L 3000:localhost:3000 tennisl1
}

chrome-proxy() {
	# https://www.chromium.org/developers/design-documents/network-stack/socks-proxy/
	google-chrome-stable --proxy-server="socks5://localhost:1137" --host-resolver-rules="MAP * ~NOTFOUND , EXCLUDE localhost" --profile-directory="Profile 1"
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

# https://github.com/jarun/nnn/blob/master/misc/quitcd/quitcd.bash_zsh
n() {
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

agr() {
	ag -0 -l "$1" | xargs -0 perl -pi -e "s/$1/$2/g"
}

agrbak() {
	ag -0 -l "$1" | xargs -0 perl -pi.bak -e "s/$1/$2/g"
}

datemili() {
	milis="$1"
	date -d@$((milis / 1000))
}

exportcreds() {
	credname="${1:-exports}"
	credfile="$HOME/.${credname}.creds"
	if [[ ! -e "$credfile" ]]; then
		echo "${credfile} not found" 1>&2
		return 1
	fi
	echo "using cred file $credfile" 1>&2
	source "$credfile"
}
