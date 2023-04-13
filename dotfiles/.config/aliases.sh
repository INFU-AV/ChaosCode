##### ALIASES:

## WSL stuff
alias Exp='explorer.exe `wslpath -w "$PWD"`'
alias Agi="sudo apt-get install"
alias Agn="sudo apt-get install --no-install-recommends"
alias As="apt search"
alias Arm="sudo apt remove"
alias Aar="sudo apt autoremove"
alias n="nvim"
## pkg shortcut heaven
alias Pu="pkg upgrade ; pkg clean ; pkg autoclean"
alias Ps="pkg search"
alias Psh="pkg show"
alias Pin="pkg install"
alias Prm="pkg remove"
alias Ph="cat $PREFIX/var/log/apt/history.log"
alias Pli="pkg list-installed"
alias Pla="pkg list-all"

## Navigation
alias ..="cd .."
alias cd..="cd .."
alias cd...="cd ../.."
# Easy quitting
alias q="exit"
alias Q="pkill emacs ; pkill ttyd ; exit"
alias QQ="kill -9 -1"
alias kk="exit" #for other side lol

alias c="clear"

## Apps
alias ncdu="ncdu -q"
# this one requires aliases.sh to be "chmod +x"
alias myTt="ENV=~/.config/aliases.sh ttyd -c Infu: sh"
PS1='-]:INFU:[- '

alias diskspace="du -S | sort -n -r | less"

alias ls='ls -h --color=auto --group-directories-first' # Alphabetical
alias lS='ls -h --color=auto --group-directories-first -S -s -F' # largest files first 
alias lx='ls -h --color=auto --group-directories-first -o -X' # by extension, detail
alias lsa='ls -h --color=auto --group-directories-first -a'
alias lsr='ls -h --color=auto --group-directories-first -r'
alias lsc='ls -h --color=auto -c'
# -a all files
# -c creation time
# -1 for 1 thing per line
# -r to reverse list
# -R for recursive

## my Emacs aliases
alias e="emacsclient -c"
alias es="emacsclient --create-frame --alternate-editor="""
alias eX="pkill emacs"
alias DeDup="fdupes -r -o name -i . -d -N"
alias Home="am start -W -c android.intent.category.HOME -a android.intent.action.MAIN"

# alias ip="ifconfig | grep inet"
alias ip="ifconfig 2>/dev/null | grep 'inet 192.168' | head -1 | awk '{print \$2}'"
# https://filebrowser.org/installation
alias serve="python -m http.server" # serve files through python
alias serve="filebrowser -p 20666 -r ."