#!/usr/bin/env bash

# Main repo:
# https://github.com/INFU-AV/ChaosCode

# WHAT IT DOES:
# This script lets you have multiple Extra-Keys setups
# doing so by swapping termux.properties file with
# one of specified configurations in this script:
# "ExtraKeySet" configs + "SharedOptions" at the end!

# HOW IT WORKS:
# Script checks first line of $HOME/termux.properties file
# and if value is correct, it overwrites termux.properties
# by parsing ExtraKeySet(X+1) + SharedOptions functions to it
# (where X is currently used set, looping back to 1st one)
# and if there's no match: backup current one + swap to 1st config!

# ( Extra Key Sets are written one key per line, with )
# ( weird ergonomic decisioms of mine, Emacs combos & )
# ( lil' inconsistent syntax to show what is allowed, )
# ( what gets skipped - Hope it's good to learn from! )

main() {
# Navigate where you want to go by headers:
##### CONFIG1
##### CONFIG2
##### CONFIG3
##### SHARED OPTIONS
##### VARIABLES & SWAPPING MECHANISM

##### CONFIG1
ExtraKeySet1 () { # emoji-keys half-joke set lol
cat > $PropertiesFile << End1
# SET1

# please do NOT modify first line at all, Extra-Keys swapping script depends on it!

extra-keys = [[ \
{key: 'ESC', display: '❌️', popup: {macro: 'ALT x recompile RETURN', display:'compile!'}} \
, \
{key: ':', display: '➗',popup: {macro: KEYBOARD}} \
, \
{key: '/', display: '🗿',popup: {macro: '|'}} \
, \
{key: 'LEFT', display: '⬅️',popup: {macro: "<<"}} \
, \
{key: 'UP', display: '⬆️',popup: {macro: "HOME"}} \
, \
{key: 'RIGHT', display: '➡️',popup: {macro: ">>"}} \
], [ \
], [ \
{key: 'TAB', display: '♻️', popup: {macro: "SHIFT TAB", display:'S-TAB'}} \
, \
{key: 'CTRL', display: '🇨', popup: {macro: 'ESC ,', display:'INFU eMAP'}} \
, \
{key: 'ALT', display: '🇲', popup: {macro: '| termux-copy-clipboard', display:'Tclip Pipe'}} \
, \
{key: '-', display: '💞', popup: {macro: '_',popup: {macro: '+'}}} \
, \
{key: 'DOWN', display: '⬇️',popup: {macro: "END"}} \
, \
{key: 'DEL', display: '💩',popup: {macro: 'ALT x shell-command RETURN swap RETURN ALT x shell-command RETURN termux-reload-settings RETURN', display:'swap'}} \
]]
# {key: 'END',popup: {macro: PASTE }}, \
# {key: ''}
terminal-toolbar-height = 1.0
terminal-margin-horizontal = 20
End1
}

##### CONFIG2
ExtraKeySet2 () { # 3row dual-use key set I use often
cat > $PropertiesFile << End2
# SET2

# please do NOT modify first line at all, Extra-Keys swapping script depends on it!

extra-keys = [[ \
{key: 'ESC',popup: {macro: 'ALT x recompile RETURN',display:"compile!"}} \
, \
{key: 'F10',display: "f10", popup: 'F2'} \
, \
{key: '|',display: '| ^⌨️', popup: KEYBOARD } \
, \
{key: 'HOME',popup: {macro: 'F6'}} \
, \
{key: 'UP',popup: {macro: "PGUP"}} \
, \
{key: 'END',popup: {macro: "F8"}} \
], [ \
{macro: "CTRL x @ h",display: 'H-👁️'} \
, \
{key: ':',display: ': ^"', popup: {macro: '"'}} \
, \
{key: '/',display: '/ ^\\\\\\\\', popup: {macro: 'BACKSLASH' }} \
, \
{key: 'LEFT',display: '← ^<<', popup: {macro: "<<"}} \
, \
{key: 'DOWN',popup: {macro: "PGDN"}} \
, \
{key: 'RIGHT',display: '→ ^>>', popup: {macro: ">>"}} \
], [ \
{key: 'TAB',display: 'tab', popup: {macro: 'SHIFT TAB',display:'S-TAB'}} \
, \
{key: 'CTRL',display: 'Ctrl ^$', popup: {macro: '$'}} \
, \
{key: 'ALT',display: 'Alt ^~', popup: {macro: '%',display:'%'}} \
, \
{key: '-',display: "- ^_", popup: {macro: '_',popup: {macro: '+'}}} \
, \
{key: 'SHIFT',display: 'Shift',popup: {macro: ''}} \
, \
{key: 'DEL',popup: {macro: 'ALT x shell-command RETURN swap RETURN ALT x shell-command RETURN termux-reload-settings RETURN',display:'swap'}} \
]]
# ['']]
# {key: 'END',popup: {macro: PASTE }}, \

terminal-toolbar-height = 0.95
terminal-margin-horizontal = 0
extra-keys-text-all-caps = false
End2
# note the absolutely ridiculous
# backlash escaping - it's done so
# to not only properly escape native
# termux config, but `cat` itself!
}
##### CONFIG3
ExtraKeySet3(){ # No ExtraKeys
cat > $PropertiesFile << End3
# SET3

# please do NOT modify first line at all, Extra-Keys swapping script depends on it!

extra-keys = [[ ]]
End3
}
##### SHARED OPTIONS
SharedOptions() { # additional settings shared by all configs
cat >> $PropertiesFile << EOF

terminal-cursor-blink-rate = 250
terminal-onclick-url-open = true
bell-character = vibrate
volume-keys = volume
extra-keys-text-all-caps = false
use-black-ui = true
disable-hardware-keyboard-shortcuts = true
allow-external-apps = true
run-termux-am-socket-server = true

# https://github.com/INFU-AV/ChaosCode
EOF
}
##### VARIABLES & SWAPPING MECHANISM
SwappingTo(){ # Swaps to KeySet specified by 1st arg
  ExtraKeySet"$1"
  SharedOptions
  exec termux-reload-settings
} # we gonna use it later!

# Number of Extra-Keys sets we got in this script:
# IF YOU ADD CONFIG, UPDATE THIS NUMBER! 
local SetAmount=3
## variable for the main file cause I type it so often
local PropertiesFile="$HOME/.termux/termux.properties"
if [[ ! -e $PropertiesFile ]]; then
  echo '"~/.termux/termux.properites file is not present, aborting...'
  exit 2
fi 
# Find out what is the set number
local FirstLine="$(head -1 $PropertiesFile)"
local SetNumber="${FirstLine#\# SET}"

# final doublechecking that the value:
# Has lenghth of 1;
# Is a number;
# Is greater than 0;
if [[ ${#SetNumber} = 1 && \
$SetNumber == $(( $SetNumber + 0 )) && \
$SetNumber -gt 0 ]]; then
  # That's definitely config made by this script, lets swap!
  if [[ $SetNumber -lt $SetAmount ]]; then
    printf -- "Set $SetNumber detected! Changing to ExtraKeySet$(( $SetNumber + 1 ))...\n"
    SwappingTo $(( $SetNumber + 1 ))
  elif [[ $SetNumber -ge $SetAmount ]]; then
    printf -- "Looping back to ExtraKeySet1...\n"
    SwappingTo 1
    fi
else
  printf -- "termux.properties file does NOT match ones in the script:\n"
  printf -- "Would you like to force-swap it to first one?\n"
  printf -- "  (Don't worry, current one will be backed up!)\n"
  while true; do
    read -n1 -p "y/Y = Yes ; n/N = No "
    case $REPLY in # this checks only for first letter of response
      [Yy]*) printf -- "\nBacking up  + Swapping termux.properties now!\n"
             cp $PropertiesFile ${PropertiesFile}.old
             SwappingTo 1
             exit
             ;;
      [Nn]*) printf -- "\nCancelling process...\n"
             exit
             ;;
          *) printf -- "Please answer y/Y for Yes, n/N for No:.";;
    esac
  done
fi
} ; main