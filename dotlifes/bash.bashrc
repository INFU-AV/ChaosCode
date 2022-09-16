#!/data/data/com.termux/files/usr/bin/bash

# Backup motd lmao
# http://patorjk.com/software/taag/#p=display&f=Graffiti&t=INFU
# nr1 from https://fsymbols.com/text-art/
if [[ ! -e "$PREFIX/etc/motd" ]] ; then cat <<END
.___ _______  _______________ ___  
|   |\      \ \_   _____/    |   \ 
|   |/   |   \ |    __) |    |   / 
|   /    |    \|     \  |    |  /  
|___\____|__  /\___  /  |______/   
            \/     \/
END
fi

# Command history tweaks:
# - Append history instead of overwriting
#   when shell exits.
# - When using history substitution, do not
#   exec command immediately.
# - Do not save to history commands starting
#   with space.
# - Do not save duplicated commands.
shopt -s histappend
shopt -s histverify
export HISTCONTROL=ignoreboth
# No backlashes in bash next to $locations:
shopt -u progcomp

    # customised PS1 prompt via https://github.com/jmatth/ezprompt
# Show max 2 dirs in path
PROMPT_DIRTRIM=2
# Don't display error code if 0
function nonzero_return() {
	RETVAL=$?
	[ $RETVAL -ne 0 ] && echo "-$RETVAL-"
}

export PS1="\[\e[35m\]\A\[\e[m\]\[\e[33;41m\]\`nonzero_return\`\[\e[m\] \w\[\e[32m\]\\$\[\e[m\] "

# Handles nonexistent commands.
# If user has entered command which invokes non-available
# utility, command-not-found will give a package suggestions.
if [ -x /data/data/com.termux/files/usr/libexec/termux/command-not-found ]; then
	command_not_found_handle() {
		/data/data/com.termux/files/usr/libexec/termux/command-not-found "$1"
	}
fi
    
    # Aliases

# Navigation
alias cd.="cd .."
alias cd..="cd .."
alias ..="cd .."
alias ...="cd ../.."
# Easy quitting
alias q="exit"
alias Q="exit"
alias kk="exit" #for other side lol
# pkg shortcut heaven
alias Pu="pkg update ; pkg clean ; pkg autoclean"
alias Ps="pkg search"
alias Pin="pkg install"
alias Prm="pkg remove"
alias Ph="cat $PREFIX/var/log/apt/history.log"
alias Pli="pkg list-installed"
alias Pla="pkg list-all"

alias c="clear"

    # Apps
alias n="nano -m" #Mouse support
alias ncdu="ncdu -q"
# personalized neofetch
alias Neo="neofetch --off --disable title --cpu_speed on --cpu_temp C --memory_unit gib --uptime_shorthand tiny " # --color_blocks off"

# turn "ls" into "exa" for faster colourful usage
alias ls="exa   -aF  --group-directories-first"
alias lsr="exa  -aF  --group-directories-first -R"
alias lst="exa  -aF  --group-directories-first -T --level=3"
alias lso="exa  -aF1 --group-directories-first"
alias lsor="exa -aF1 --group-directories-first -R"
alias lsot="exa -aF1 --group-directories-first -T --level=3"
alias lsm="exa  -aF  --group-directories-first -s newest"
alias lsmr="exa -aF  --group-directories-first -s newest -R"
alias lsmt="exa -aF  --group-directories-first -s newest -T --level=3"
    
    # follow every "cd" command with "ls"
function cd() {
    DIR="$*";   
if [ $# -lt 1 ]; then
    DIR=$HOME;  # if no DIR given, go home
fi;
builtin cd "${DIR}" && \
    ls  # <- your preffered ls command
} # mine is aliased to exa anyway lol
    
    # requires Termux:API
alias send="termux-share -a send" # send your files around!
alias clip="termux-clipboard-set" # use pipe before!

    # quick termux clipboard
function cx() {
  if [[ -n $* ]]
then
    echo "$*" | termux-clipboard-set
    echo '[copied!]'
else
    echo '[cx usage:"]'
    echo '[cx (string you want to copy)]'
    echo '[AVOID BACKLASHES!]'
fi
}

    # ^ same, + backticks for Discord-code block!
function cX() {
if [[ -n $* ]]
then
    echo "\`\`\`$*\`\`\`" | termux-clipboard-set
    echo '[copied!]'
else
    echo '[cX (+bonus backticks!) usage:"]'
    echo '[cX (string you want to copy)]'
    echo '[AVOID BACKLASHES!]'
fi
}

    # my Emacs aliases
alias e="emacs"
alias E="emacs -l ~/.emacs.d/E.el"
# alias E="emacs -f dired-jump -f split-window -f term"
alias ep="emacs -f list-packages"
alias ei="emacs --debug-init ~/.emacs.d/early-init.el ~/.emacs.d/init.el -f use-package-report -f split-window"
alias et="emacs -l ~/.emacs.d/do.el ~/xinfu/QN.org ~/xinfu/todo.org"
alias ec="emacs -NW"

# QuickTars, as I never remmember syntax
alias tar?=cat <<EOF
=====HOW TO TAR=====
EXAMPLE: tar czvf archive.tar.gz directory
 first flag "c" is for compressing
    use x for extraction
    use t  to view archive
 2nd flag z packs your .tar into .gz
    use j to pack it into .bz2
 3rd flag v is for verbose output
 LAST flag f asks for output archive name
    and folder/file to be packed
===== ===== =====
EOF

alias ip="ifconfig | grep inet"

#Flashy intro sequence lmao
RED='\033[1;31m'
NC='\033[0m' # NoColor
YW='\033[1;33m'
echo -e -n "${YW}=====${RED}[INFU_HAS_LOGGED_IN]${YW}=====${NC}"
echo 
Neo
dice() {
throw=$(( $RANDOM % 10 ))
}
dice #initiate random number
echo -e "${YW}=====The Lucky Number:${NC}[${RED}"$throw"${NC}]${YW}=====${NC}"


# phone locations I use often:
exports() {
export dl=~/storage/downloads
export ex=~/storage/external-1
export sc=~/xinfu/scripts/
export piggy=~/storage/shared/PSP/GAME/Piggy
export za=/storage/emulated/0/Android/data/it.dbtecno.pizzaboypro/files/pizzaboy/save
export gb=/storage/3439-6335/INFU/ARTS/GBcamera
export -f cx
export -f cX
export EDITOR="emacs"
}
# Only export those in first shell
[[ $SHLVL == 1 ]] && exports

# Loop to start-up Emacs
# but not while inside Emacs!
if [[ -z "$INSIDE_EMACS" ]]
then
    emacs -l ~/.emacs.d/EE.el
else
unalias e
fi
