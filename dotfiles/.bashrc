#!/usr/bin/env bash

# Main repo:
# https://github.com/INFU-AV/ChaosCode

# Navigate Headers:
##### ALIASES
##### EXPORTS & FUNCTIONS
##### LOAD EMACS DAEMON
##### TITLE ZONE
##### BASH SPECIFIC
##### PROMPTLINE dice-or-error-display!

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

##### ALIASES
#
if [ -f "$HOME/.config/aliases.sh" ]; then
    chmod +x "$HOME/.config/aliases.sh"
    source "$HOME/.config/aliases.sh"
else
    echo "No aliases to load!"
fi
# -ALIAS_END #

##### EXPORTS & FUNCTIONS
#
cd() { # follow every "cd" command with "ls"
    local DIR="$*"
    if (( ! "$#" )); then
        DIR=$HOME; # if no DIR given, go home
    fi
    builtin cd "${DIR}" && ls # <- your preferred ls command
} # it will also take aliases from above

mkcd() { # Make folder and enter it
NAME=$1; mkdir -p "$NAME"; cd "$NAME"; }

# lsH() { # don't show hidden, but count them (works wonky)
#   local DIR="$*"
#   (( ! "$#" )) && DIR=$PWD; # if no DIR given, do $PWD
# exa -F --group-directories-first -s modified $DIR
# echo "command ls "$*" | wc -l) hidden]"
# # echo "["$(($(command ls $DIR -A | wc -l) - $(command ls $DIR | wc -l)))" hidden]"
# } ; alias ls=lsH
# would be cool to have ls that reacts on
# previous ls command, becoming "ls -A"

cx() { # quick termux clipboard
    if [[ -n "$*" ]]; then
        echo -e "${*//\\/\\\\}" | termux-clipboard-set
        echo '[copied!]'
    else
        printf '%b' \
        '[cx usage:"]' \
        '\n[cx (string you want to copy)]\n'
    fi
}

cX() { # same as function above, with backticks for Discord codeblock
if [[ -n "$*" ]]; then
    echo -e "\`\`\`\n${*//\\/\\\\}\n\`\`\`" | termux-clipboard-set
    echo '[copied!]'
else
    printf '%b' \
    '[cX (+bonus backticks!) usage:"]' \
    '\n[cX (string you want to copy)]\n'
fi
}

# set OS-specific clipboard shortcuts
if [[ "$OSTYPE" == "linux-android" ]]; then
alias Clp="termux-clipboard-get"
alias Clc="termux-clipboard-set" # use with pipe!
elif [[ -n "$WSL_DISTRO_NAME" ]]; then
alias Clp="powershell.exe Get-Clipboard"
alias Clc="clip.exe" # use with pipe!
fi

exports() { # personal phone locations I use often:
# if no SD card on phone (my new phone), then
# I wanna use EXTRA folder outside Termux!
if [[ -d "$HOME/storage/external-1" ]]
  then
       export ex="$HOME/storage/external-1"
  else export ex="$HOME/storage/shared/EXTRA"
fi
export dl="$HOME/storage/downloads"
export sc="$HOME/xinfu/scripts/"
export piggy="$HOME/storage/shared/PSP/GAME/Piggy"
export za="/storage/emulated/0/Android/data/it.dbtecno.pizzaboypro/files/pizzaboy/save"
export gb="/storage/3439-6335/INFU/ARTS/GBcamera"
export psx="$HOME/storage/shared/duckstation"
## requires Termux:API
nv(){ # NotifViewer: view images in notification
termux-notification --id 777 -c "\"${1##/}"\" --image-path $(realpath "$1") --alert-once --type media
}
alias send="termux-share -a send" # send your files around!
alias V="termux-share -a view"
alias To="termux-torch on"
alias Tf="termux-torch off"
# swap out my termux button bar for another
alias ExtraBar="bash ~/.termux/swap.sh ; termux-reload-settings"
export EDITOR="emacsclient --create-frame --alternate-editor="""
}
# Only export those in first shell
# and only on my android device
# must be before I start Emacs server
# otherwise I can't access those shortcuts
[[ "$OSTYPE" == "linux-android" && $SHLVL == 1 ]] && exports

export PATH=$PATH:~/bin


##### LOAD EMACS DAEMON
LoadEmacsDaemon(){ # Check if Emacs daemon is running
# if not, launch it!
if ! emacsclient -e 0 >&/dev/null
then emacs --daemon &
# else emacsclient -c "$@"
fi ; } ; command -v emacs >&/dev/null && LoadEmacsDaemon
# Actual switching onto Emacs on boot is
# at the very end of this config file!

##### TITLE ZONE
#
# Backup motds lmao
# https://fsymbols.com/text-art/
#  ██╗███╗░░██╗███████╗██╗░░░██╗
#  ██║████╗░██║██╔════╝██║░░░██║
#  ██║██╔██╗██║█████╗░░██║░░░██║
#  ██║██║╚████║██╔══╝░░██║░░░██║
#  ██║██║░╚███║██║░░░░░╚██████╔╝
#  ╚═╝╚═╝░░╚══╝╚═╝░░░░░░╚═════╝░
# http://patorjk.com/software/taag/#p=display&f=Graffiti&t=INFU
# .___ _______  _______________ ___
# |   |\      \ \_   _____/    |   \
# |   |/   |   \ |    __) |    |   /
# |   /    |    \|     \  |    |  /
# |___\____|__  /\___  /  |______/
#             \/     \/

scanline() { # Prints out all arguments with custom delay(sleep) between each
#Thanks TomIO for creating it!
#TODO: modify it so it outputs char-by-char so I don't have to separate all that ASCII like that
   while (( "$#" )); do
       printf '%b' "$1"
       sleep 0.03
       shift
   done
return
}
if [[ $SHLVL == 1 && -z "$INSIDE_EMACS" ]]
then scanline "\n" \
    " ██╗" "███╗░░██╗" "███████╗" "██╗░░░██╗\n" \
    " ██║" "████╗░██║" "██╔════╝" "██║░░░██║\n" \
    " ██║" "██╔██╗██║" "█████╗░░" "██║░░░██║\n" \
    " ██║" "██║╚████║" "██╔══╝░░" "██║░░░██║\n" \
    " ██║" "██║░╚███║" "██║░░░░░" "╚██████╔╝\n" \
    " ╚═╝" "╚═╝░░╚══╝" "╚═╝░░░░░" "░╚═════╝░\n\n"
else scanline "\n" \
    ".___ _______  ""_______________ ___  \n" \
    "|   |\      \ ""\_   _____/    |   \ \n" \
    "|   |/   |   \\"" |    __) |    |   / \n" \
    "|   /    |    \\""|     \  |    |  /  \n" \
    "|___\____|__  /""\___  /  |______/   \n" \
    "            \/ ""    \/              \n\n"
fi
# I removed original Termux banners with this:
# rm $PREFIX/etc/motd*

##### BASH SPECIFIC
#
### Bash resources:
# https://tldp.org/LDP/abs/html/
# No backlashes in bash next to $locations:
    # shopt -u progcomp
# non-invasive helpers when cd'ing into places
    shopt -s cdable_vars
    shopt -s cdspell
    shopt -s dirspell
# show me stopped jobs when exiting
    shopt -s checkjobs
# stops the "Display all 1482 possibilities? (y or n)"
# when TAB'ing on empty cmd line
    shopt -s no_empty_cmd_completion
# Custom location/settings for .bash_history file:
    export HISTFILE=$HOME/.config/.bash_history
# amount of commands stored in bash memory at once
    export HISTSIZE=1000
# and here's how many are stored in history file!
    export HISTFILESIZE=2000
# append to the history file, don't overwrite it
    shopt -s histappend
# ignore those exact matches:
    export HISTIGNORE="&:bg:fg:ls"
# don't put duplicate lines in the history
# AND ignore lines starting with space
    export HISTCONTROL="erasedups:ignorespace"
# Don't wanna see .lesshst file at all
    export LESSHISTFILE="/dev/null"
# use TAB/S-TAB to cycle through files
    bind TAB:menu-complete
    bind '"\e[Z":menu-complete-backward'
# show candidates before cycling
    bind "set show-all-if-ambiguous on"
    bind "set menu-complete-display-prefix on"
# When putting down closing parenthesis, matching one will blink
    bind "set blink-matching-paren on"
# Completion be as coloured as "ls"
    bind "set colored-stats on"
    bind "set colored-completion-prefix on"

##### PROMPTLINE dice-or-error-display!
#
# if no error:
 # display random 0-9 number (decorative)
# if error:
 # display error code until successful command
dice-or-error-prompt() {
 local RETVAL="$?"
 local SoDice="$((RANDOM % 10))"
 if ((RETVAL)); then
PS1="\[\e[35m\]\A\[\e[m\]\w\[\e[33;41m\]-$RETVAL-\[\e[m\]"
 else
PS1="\[\e[35m\]\A\[\e[m\]\w\[\e[36m\][\[\e[m\]$SoDice\[\e[36m\]]\[\e[m\]"
fi ; } ; PROMPT_COMMAND="dice-or-error-prompt;history -a"
### Explanation:
## RETVAL preserves error code
# because "Dice roll" counts as action
# and it's gonna end in success
## 2 different PS1 to prevent glitching
# history -a saves history immediately
#
## initial prompt + error checking idea:
# https://github.com/jmatth/ezprompt
## Good show-off prompt with clock in top-right
# https://tldp.org/HOWTO/Bash-Prompt-HOWTO/clockt.html
PS2='» '
PROMPT_DIRTRIM=2
##### LESS
# Set colors for less. Borrowed from:
# https://wiki.archlinux.org/index.php/Color_output_in_console#less
export LESS='-R --use-color -Dd+r$Du+b'
# export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANPAGER="less -R --use-color -DC+B -DE+Rk -DM+Y -DP+WK -DS+Yk -Dd+m -Dk+m -Ds+y" # -Du+s"
[[ "$OSTYPE" == "linux-android" && $SHLVL == 1 ]]

LMF(){ # Output full path of last modified file in current directory
  #or in path supplied by 1st argument
  # usage: LastModifiedF  ile $HOME/downloads/
realpath "$(stat -c '%Y %n' "${1:-.}"/* | sort -k1,1nr | head -1 | awk '{print $2}')"
# https://askubuntu.com/questions/684220/how-to-list-the-last-modified-files-in-a-specific-directory-recursively
}
NameSwap() { # Swap filename $1 with $2, if they exist (from Uzi's bashrc).
# swaps 
  local TMPFILE=tmp.$$
  [ "$#" -ne 2 ] && echo "swap: 2 arguments needed" && return 1
  [ ! -e "$1" ] && echo "swap: $1 does not exist"  && return 1
  [ ! -e "$2" ] && echo "swap: $2 does not exist"  && return 1
  mv "$1" $TMPFILE
  mv "$2" "$1"
  mv $TMPFILE "$2"
}

Ff() { # Find a file with a pattern in name:
find . -type f -iname '*'"$*"'*' -ls ; }
Fp(){ # same as above, just print only filename:
find . -type f -iname '*'"$*"'*' -print ;}
Fe() { # Find a file with pattern $1 in name and Execute $2 on it:
find . -type f -iname '*'"${1:-}"'*' \
 -exec ${2:-file} {} \;  ; }

# since we got spare time till Emacs turns on..
# Flashy intro sequence lmao
#
FlashyIntro() {
  local RE='\e[0;31m' # REd
  local NC='\e[0;m'   # NoColor
  local GR='\e[1;32m' # GReen
  local YW='\e[5;33m' # YelloW
  local CA='\e[1;36m' # CyAn
  printf -- "${YW}=====${RE}[INFU_LEVEL:${NC}${SHLVL}${RE}]${YW}=====${NC}\n"
  printf -- "${GR}Current date${NC}: " ; date +%A,%F
  printf -- "${GR}\$HOME status${NC}: "
  if [[ $(command ls -a $HOME | wc -l) -gt 20 ]]
    then printf "🚫" ; fi ; printf "✨\n"
  command -v emacs >&/dev/null && \
  printf -- "${GR}Emacs packages${NC}: "
  printf -- "$(grep -c 'use-package' $HOME/.emacs.d/init.el)\n"
  if ((0)); then printf -- "${GR}Sober${NC}: \
    $(( ($(date +%s) - $(date +%s -ud '2023-03-10 13:00:00'))/3600/24)) days\n"; fi
  printf -- "${GR}Kernel Name${NC}: " ; uname -s 
  printf -- "${GR}Nodename${NC}: " ; uname -n 
  # printf -- "${GR}Kernel Release${NC}:" ; uname -r 
  printf -- "${GR}Machine${NC}: " ; uname -m 
  printf -- "${GR}Current OS${NC}: " ; uname -o 
  [[ $SHLVL == 1 ]] && printf -- "${GR}Local IP${NC}: $(ifconfig 2>/dev/null \
    | grep 'inet 192.168' | head -1 | awk '{print $2}')\n"
  printf -- "${GR}------  -----  ----  ---- ${NC}\n"
  printf -- MEMORY"$(free --mega -h -t)" | column -t | cut -c 1-26
  printf -- "${GR}------  -----  ----  ---- ${NC}\n"
  printf -- "${GR}\"${NC}$(uptime -p)${GR}\"${NC}\n"
  printf -- "${YW}=====The Lucky Number:${NC}[${RE}"$((RANDOM % 10))"${NC}]${YW}=====${NC}\n"
} ; FlashyIntro


bootmacs() {
# Loop to start-up Emacs
# but not while inside Emacs!
if [[ -z "$INSIDE_EMACS" ]]; then
# additional check for my WSL to not launch more emacs
[[ -z "$WSL_DISTRO_NAME" ]] && emacsclient -c
    echo ""
else
    echo "Already inside Emacs!"
    unalias e
# can open files like that in term!
    alias e="emacsclient -n"
    alias eq="emacs -Q"
    echo "Remapping aliases.."
fi
} ; bootmacs

# https://infu.fyi/