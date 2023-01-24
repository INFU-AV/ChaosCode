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

swap() { # Termux Extra-Keys swapper
"$HOME/.termux/swap.sh"
} # using it as macro on extra-keys

exports() { # personal phone locations I use often:
export dl="$HOME/storage/downloads"
export ex="$HOME/storage/external-1"
export sc="$HOME/xinfu/scripts/"
export piggy="$HOME/storage/shared/PSP/GAME/Piggy"
export za="/storage/emulated/0/Android/data/it.dbtecno.pizzaboypro/files/pizzaboy/save"
export gb="/storage/3439-6335/INFU/ARTS/GBcamera"
export psx="$HOME/storage/shared/duckstation"
export EDITOR="emacs"
}
# Only export those in first shell
# and only on my android device
# must be before I start Emacs server
# otherwise I can't access those shortcuts
[[ "$OSTYPE" == "linux-android" && $SHLVL == 1 ]] && exports

##### LOAD EMACS DAEMON
#
# Check if daemon is on already
if ! emacsclient -e 0 >&/dev/null
then emacs -nw --no-x-resources --daemon &
# else emacsclient -c "$@"
fi

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

scanline() { # roll out banners bit by bit
   while (( "$#" )); do
       printf '%b' "$1"
       sleep 0.02
       shift
   done
return
}
#
if [[ $SHLVL == 1 && -z "$INSIDE_EMACS" ]]
then scanline "\n" \
    " ██╗" "███╗░░██╗" "███████╗" "██╗░░░██╗\n" \
    " ██║" "████╗░██║" "██╔════╝" "██║░░░██║\n" \
    " ██║" "██╔██╗██║" "█████╗░░" "██║░░░██║\n" \
    " ██║" "██║╚████║" "██╔══╝░░" "██║░░░██║\n" \
    " ██║" "██║░╚███║" "██║░░░░░" "╚██████╔╝\n" \
    " ╚═╝" "╚═╝░░╚══╝" "╚═╝░░░░░" "░╚═════╝░\n\n"
else scanline "\n" \
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
#
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
    export HISTCONTROL="erasedups:ignorespace"
# Don't wanna see .lesshst file at all
    export LESSHISTFILE="/dev/null"
# use TAB/S-TAB to cycle through files
    bind TAB:menu-complete
    bind '"\e[Z":menu-complete-backward'
# show candidates before cycling
    bind "set show-all-if-ambiguous on"
    bind "set menu-complete-display-prefix on"

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
fi
} ; PROMPT_COMMAND="dice-or-error-prompt;history -a"
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

##### LESS
# Set colors for less. Borrowed from:
# https://wiki.archlinux.org/index.php/Color_output_in_console#less
export LESS='-R --use-color -Dd+r$Du+b'
# export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANPAGER="less -R --use-color -DC+B -DE+Rk -DM+Y -DP+WK -DS+Yk -Dd+m -Dk+m -Ds+y"
# since we got spare time till Emacs turns on..
# Flashy intro sequence lmao
#
FlashyIntro() {
local RE='\e[0;31m' # REd
local NC='\e[0m'    # NoColor
local GR='\e[1;32m' # GReen
local YW='\e[5;33m' # YelloW
local CA='\e[1;36m' # CyAn
echo -e -n "${YW}=====${RE}[INFU_LEVEL:${NC}${SHLVL}${RE}]${YW}=====${NC}"
echo
echo
echo -e -n "${GR}HOME folder status${NC}: " ; if [[ $(command ls -a $HOME | wc -l) -gt 20 ]]; then printf "🚫" ; fi ; printf "✨\n"
# sneaky ripgrep dependency lol
echo -e -n "${GR}Emacs packages${NC}: $(rg -c 'use-package' ~/.emacs.d/init.el)"
echo
echo -e -n "${GR}Sober${NC}: $(( ($(date +%s) - $(date +%s -ud '2023-01-08 00:00:00'))/3600/24)) days"
[[ "$OSTYPE" == "linux-android" ]] && neofetch --off --disable title --cpu_speed on --cpu_temp C --memory_unit gib --uptime_shorthand tiny --no_config # --color_blocks off
echo -e "${YW}=====The Lucky Number:${NC}[${RE}"$((RANDOM % 10))"${NC}]${YW}=====${NC}"
} ; FlashyIntro

[[ "$OSTYPE" == "linux-android" && $SHLVL == 1 ]]
# Get rid of system motd's
[[ -e $PREFIX/etc/motd ]] &&  rm -- $PREFIX/etc/motd*
# Same with that home history file
[[ -e ~/.bash_history ]] && rm -f -- ~/.bash_history
# ..And "less", I couldn't care any less
[[ -e ~/.lesshst ]] && rm -f -- ~/.lesshst

bootmacs() {
# Loop to start-up Emacs
# but not while inside Emacs!
if [[ -z "$INSIDE_EMACS" ]]; then
    sleep 0.5
    emacsclient -t
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