##### ALIASES:

## WSL stuff
alias Exp='explorer.exe `wslpath -w "$PWD"`'
alias Agi="sudo apt-get install"
alias Agn="sudo apt-get install --no-install-recommends"
alias As="apt search"
alias Arm="sudo apt remove"
alias Aar="sudo apt autoremove"
alias n="nvim"

## Navigation
alias ..="cd .."
alias cd..="cd .."
alias cd...="cd ../.."
# Easy quitting
alias q="exit"
alias Q="pkill emacs ; pkill ttyd ; exit"
alias QQ="kill -9 -1"
alias kk="exit" #for other side lol

## pkg shortcut heaven
alias Pu="pkg upgrade ; pkg clean ; pkg autoclean"
alias Ps="pkg search"
alias Psh="pkg show"
alias Pin="pkg install"
alias Prm="pkg remove"
alias Ph="cat $PREFIX/var/log/apt/history.log"
alias Pli="pkg list-installed"
alias Pla="pkg list-all"

alias c="clear"

## Apps
alias ncdu="ncdu -q"
# this one requires aliases.sh to be "chmod +x"
alias myTt="ENV=~/.config/aliases.sh ttyd -c Infu: sh"

## turn "ls" into "exa" for faster colourful usage
alias ls="exa    -F  --group-directories-first -s modified"
alias lsa="exa  -aF  --group-directories-first -s modified"
alias lse="exa  -aF  --group-directories-first -s extension"
alias lsr="exa  -aF  --group-directories-first -R"
alias lso="exa  -aF1 --group-directories-first -s name"
alias lsor="exa -aF1 --group-directories-first -R"

## my Emacs aliases
alias e="emacsclient -t"
alias E="emacs -l ~/.emacs.d/EdiredTerm.el"
alias ee="emacs -l ~/.emacs.d/EnonEmpty.el ~/xinfu/QN.org ~/xinfu/todo.org"
alias Em="emacs -l ~/.emacs.d/EnonEmpty.el"

alias ip="ifconfig | grep inet"
# serve files through python
alias serve="python -m http.server"

## requires Termux:API
alias send="termux-share -a send" # send your files around!
alias clip="termux-clipboard-set" # use pipe before!
alias To="termux-torch on"
alias Tf="termux-torch off"
# swap out my termux button bar for another
alias ExtraBar="bash ~/.termux/swap.sh ; termux-reload-settings"
