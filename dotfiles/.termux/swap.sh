#!/usr/bin/env bash

# Main repo:
# https://github.com/INFU-AV/ChaosCode

# This script overwrites termux.properties file with one of configurations specified in this file:
# either Lconfig1 or Rconfig2
#(it works by checking first line of termux.properties)
# after that, function "configADD" adds settings at the end of recreated properties file

# WARNING: YOUR TERMUX.PROPERTIES WILL BE OVERWRITTEN TOTALLY! 
# Navigate Headers:
##### CONFIG1
##### CONFIG2
##### SHARED OPTIONS
##### SWAPPING

##### CONFIG1
Lconfig1 () { # emoji-keys lol
cat > termux.properties << EndL
# LEFT

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
EndL
}

##### CONFIG2
Rconfig2 () { # 3row business
cat > termux.properties << EndR
# RIGHT

extra-keys = [[ \
    {key: 'ESC',popup: {macro: 'ALT x recompile RETURN', display:"compile!"}} \
	, \
    {key: 'F10', display: "f10", popup: 'F2'} \
	, \
    {key: '|', popup: KEYBOARD } \
	, \
    {key: 'HOME',popup: {macro: 'F6'}} \
	, \
    {key: 'UP',popup: {macro: "PGUP"}} \
	, \
    {key: 'END',popup: {macro: "F8"}} \
	], [ \
    {macro: "CTRL x @ h", display: '👁️'} \
	, \
    {key: ':',popup: {macro: 'x'}} \
	, \
    {key: '/',popup: {macro: 'BACKSLASH' }} \
	, \
    {key: 'LEFT',popup: {macro: "<<"}} \
	, \
    {key: 'DOWN',popup: {macro: "PGDN"}} \
	, \
    {key: 'RIGHT',popup: {macro: ">>"}} \
	], [ \
    {key: 'TAB', display: 'tab', popup: {macro: 'SHIFT TAB', display:'S-TAB'}} \
	, \
    {key: 'CTRL', display: 'ctrl', popup: {macro: 'ESC ,', display:'INFU eMAP'}} \
	, \
    {key: 'ALT', display: 'alt', popup: {macro: 'F1', display:'cum'}} \
	, \
    {key: '-',popup: {macro: '_',popup: {macro: '+'}}} \
	, \
    {key: '',popup: {macro: ''}} \
	, \
    {key: 'DEL',popup: {macro: 'ALT x shell-command RETURN swap RETURN ALT x shell-command RETURN termux-reload-settings RETURN', display:'swap'}} \
	]]
    # ['']]
    # {key: 'END',popup: {macro: PASTE }}, \

terminal-toolbar-height = 0.95
terminal-margin-horizontal = 0
extra-keys-text-all-caps = false
EndR
}

##### SHARED OPTIONS
configADD() {
cat >> termux.properties << EOFa
terminal-cursor-blink-rate=250
terminal-onclick-url-open = true
bell-character = vibrate
volume-keys = volume-keys

extra-keys-text-all-caps = false
### Force black colors for drawer and dialogs
use-black-ui = true
disable-hardware-keyboard-shortcuts = true
EOFa
}

# Rconfig2 ; configADD
# termux-reload-settings
# exit

##### SWAPPING

# first we move to right directory to swap the file
cd "$HOME/.termux/"

# then, compare first line of actual termux.properties
if [[ $(head -1 termux.properties) == "# LEFT" ]]
then
  # echo "currently using Lhanded script"
 echo "Setting R-variant:"
 echo "[       -   <---]"
Rconfig2 ; configADD
elif [[ $(head -1 termux.properties) == "# RIGHT" ]]
then
  # echo "currently using Rhanded script"
 echo "Setting L-variant:"
 echo "[--->   -       ]"
Lconfig1 ; configADD
else
echo "different files!"
fi

exec termux-reload-settings