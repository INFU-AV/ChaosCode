#!/usr/bin/env bash
# https://infu.fyi/

# Navigate Headers:
##### EXPORTS & FUNCTIONS
##### LOAD EMACS DAEMON
##### TITLE ZONE
##### BASH SPECIFIC
##### PROMPTLINE dice-or-error-display!
##### ALIASES

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

##### EXPORTS & FUNCTIONS

cd() { # follow every "cd" command with "ls"
    local DIR="$*"
    if [[ ! "$#" ]]; then
        DIR=$HOME; # if no DIR given, go home
    fi;
    builtin cd "${DIR}" && ls # <- your preferred ls command
} # it will also take aliases from above

mkcd() { # Make folder and enter it
NAME=$1; mkdir -p "$NAME"; cd "$NAME"; }

cx() { # quick termux clipboard
    if [[ -n "$*" ]]; then
        echo -e "${*//\\/\\\\}" | termux-clipboard-set
        echo '[copied!]'
    else
        printf '%b' \
        '[cx usage:"]' \
        '[cx (string you want to copy)]'
    fi
}

cX() { # same as function above, with backticks for Discord codeblock
if [[ -n "$*" ]]; then
    echo -e "\`\`\`\n${*//\\/\\\\}\n\`\`\`" | termux-clipboard-set
    echo '[copied!]'
else
    printf '%b' \
    '[cX (+bonus backticks!) usage:"]' \
    '[cX (string you want to copy)]'
fi
}

exports() { # personal phone locations I use often:
export dl="$HOME/storage/downloads"
export ex="$HOME/storage/external-1"
export sc="$HOME/xinfu/scripts/"
export piggy="$HOME/storage/shared/PSP/GAME/Piggy"
export za="/storage/emulated/0/Android/data/it.dbtecno.pizzaboypro/files/pizzaboy/save"
export gb="/storage/3439-6335/INFU/ARTS/GBcamera"
export psx="$HOME/storage/shared/duckstation"
}
# Only export those in first shell
# and only on my android device
# must be before I start Emacs server
# otherwise I can't access those shortcuts
[[ "$OSTYPE" == "linux-android" && $SHLVL == 1 ]] && exports
export EDITOR="emacs"

# QuickTars, as I never remember syntax
tarhelp() {
cat <<EOF
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
}

##### LOAD EMACS DAEMON
# Check if daemon is on already
if ! emacsclient -e 0 >&/dev/null; then
    emacs -nw --no-x-resources --daemon &
# else emacsclient -c "$@"
fi

##### TITLE ZONE
# Backup motd lmao
# http://patorjk.com/software/taag/#p=display&f=Graffiti&t=INFU
# nr1 from https://fsymbols.com/text-art/

#  ██╗███╗░░██╗███████╗██╗░░░██╗
#  ██║████╗░██║██╔════╝██║░░░██║
#  ██║██╔██╗██║█████╗░░██║░░░██║
#  ██║██║╚████║██╔══╝░░██║░░░██║
#  ██║██║░╚███║██║░░░░░╚██████╔╝
#  ╚═╝╚═╝░░╚══╝╚═╝░░░░░░╚═════╝░

# .___ _______  _______________ ___
# |   |\      \ \_   _____/    |   \
# |   |/   |   \ |    __) |    |   /
# |   /    |    \|     \  |    |  /
# |___\____|__  /\___  /  |______/
#             \/     \/

scanline() { # roll out banners bit by bit
   while (( "$#" )); do
       printf '%b' "$1"
       sleep 0.02
       shift
   done
return
}

if [[ $SHLVL == 1 && -z "$INSIDE_EMACS" ]]; then
    scanline "\n" \
    " ██╗" "███╗░░██╗" "███████╗" "██╗░░░██╗\n" \
    " ██║" "████╗░██║" "██╔════╝" "██║░░░██║\n" \
    " ██║" "██╔██╗██║" "█████╗░░" "██║░░░██║\n" \
    " ██║" "██║╚████║" "██╔══╝░░" "██║░░░██║\n" \
    " ██║" "██║░╚███║" "██║░░░░░" "╚██████╔╝\n" \
    " ╚═╝" "╚═╝░░╚══╝" "╚═╝░░░░░" "░╚═════╝░\n\n"
else
    scanline "\n" \
    ".___ _______  _______________ ___  \n" \
    "|   |\      \ \_   _____/    |   \ \n" \
    "|   |/   |   \ |    __) |    |   / \n" \
    "|   /    |    \|     \  |    |  /  \n" \
    "|___\____|__  /\___  /  |______/   \n" \
    "            \/     \/              \n\n"
fi
# I removed original Termux banners with this:
# rm $PREFIX/etc/motd*

##### BASH SPECIFIC

### Bash resources:
# https://tldp.org/LDP/abs/html/

# No backlashes in bash next to $locations:
shopt -u progcomp
# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
shopt -s cdable_vars
shopt -s cdspell
shopt -s dirspell
# Custom location/settings for .bash_history file:
export HISTFILE=$HOME/.config/.bash_history
# amount of commands stored in bash memory at once
export HISTSIZE=50
# and here's how many are stored in history file!
export HISTFILESIZE=2000
# append to the history file, don't overwrite it
shopt -s histappend
# ignore those exact matches:
export HISTIGNORE="&:bg:fg:ls"
# don't put duplicate lines in the history
# AND ignore lines starting with space
export HISTCONTROL=ignoreboth
# Don't wanna see .lesshst file at all
export LESSHISTFILE="/dev/null"

# use TAB/S-TAB to cycle through files
bind TAB:menu-complete
bind '"\e[Z":menu-complete-backward'
# show candidates before cycling
bind "set show-all-if-ambiguous on"
bind "set menu-complete-display-prefix on"

##### PROMPTLINE dice-or-error-display!
 # if no error:
  # display random 0-9 number (decorative)
 # if error:
  # display error code until successful command
# initial prompt + error checking idea:
# https://github.com/jmatth/ezprompt

dice-or-error-prompt() {
local RETVAL="$?"
local SoDice="$((RANDOM % 10))"
if ((RETVAL)); then
PS1="\[\e[35m\]\A\[\e[m\]\w\[\e[33;41m\]-$RETVAL-\[\e[m\]"
else
PS1="\[\e[35m\]\A\[\e[m\]\w\[\e[36m\][\[\e[m\]$SoDice\[\e[36m\]]\[\e[m\]"
fi
}
### Explanation:
## RETVAL preserves error code
# because "Dice roll" counts as action
# and it's gonna end in success
## 2 different PS1 to prevent glitching
PROMPT_COMMAND="dice-or-error-prompt"

PS2='» '

# Good show-off prompt with clock in top-right
# https://tldp.org/HOWTO/Bash-Prompt-HOWTO/clockt.html

##### ALIASES:

if [ -f "$HOME/.config/aliases.sh" ]; then
    chmod +x "$HOME/.config/aliases.sh"
    source "$HOME/.config/aliases.sh"
else
    echo "No aliases to load!"
fi

# -ALIAS_END #

# since we got spare time till Emacs turns on..
#Flashy intro sequence lmao
FlashyIntro() {
local RE='\e[0;31m' # REd
local NC='\e[0m'    # NoColor
local GR='\e[1;32m' # GReen
local YW='\e[5;33m' # YelloW
local CA='\e[1;36m' # CyAn
echo -e -n "${YW}=====${RE}[INFU_LEVEL:${NC}${SHLVL}${RE}]${YW}=====${NC}"
echo
echo
echo -e -n "${GR}HOME folder status${NC}: " ; if [[ $(ls $HOME | wc -l) -gt 20 ]]; then printf "🚫" ; fi ; printf "✨\n"
echo -e -n "${GR}Emacs packages${NC}: $(grep -c 'use-package' ~/.emacs.d/init.el)"
echo
echo -e -n "${GR}Sober${NC}: $(( ($(date +%s) - $(date +%s -ud '2022-11-13 00:00:00'))/3600/24)) days"
neofetch --off --disable title --cpu_speed on --cpu_temp C --memory_unit gib --uptime_shorthand tiny --no_config --color_blocks off
echo -e "${YW}=====The Lucky Number:${NC}[${RE}"$((RANDOM % 10))"${NC}]${YW}=====${NC}"
}; FlashyIntro

# Loop to start-up Emacs
# but not while inside Emacs!
if [[ -z "$INSIDE_EMACS" ]]; then
    emacsclient -t
else
    echo "Already inside Emacs!"
fi
