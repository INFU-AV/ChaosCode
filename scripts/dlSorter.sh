#!/usr/bin/env bash

# making the script case-insensitive:
shopt -s nocaseglob

# put down path to your downloads folder!
# \/ \/ \/ R-I-G-H-T  T-H-E-R-E !!!
DownLoads=""
# /\ /\ /\ just in-between those quotes!
    # example:
    # DownLoads="/mnt/c/Users/InfuMax2/Downloads/"

howto() {
    cat >&2 <<"EOM"
  "Downloads" Directory Sorter
    -["Contain Your Chaos👁️"]-
Sort your files by extension into
multiple folders inside "_sorted"
- - ----- [CUSTOMISE!] ----- - -
You NEED to set the PATH manually!
OPEN the script, and by the top put
full location to that messy folder!
- - ----- [EXPAND ME!] ----- - -
Feel free to expand "dlSorter" by
adding extensions of things to sort!

Thanks fghsgh, NyaNyaCutie for help!
----- - ===== INFU ===== - -----
EOM
}

# uncomment for verbose script
# set -x

## Show help when there's any argument:
 #! should parse few arguments!
    # if [[ "$1" == force ]]...
    # JustDoIt="PlsDontPester"
if [[ "$#" -ne 0 ]] ; then
    howto
    exit 1
fi

## check if folder was specified, exists and is writable:
if [[ -n "$DownLoads" &&  -d "$DownLoads" && -w "$DownLoads" ]]
then
    echo "Directory accessible!"
else
    echo "No valid path recognised!"
    echo "please read help file:"
    howto
    exit 1
fi 
trblshtng() { ## troubleshooting stats
echo "troubleshooting start:"
echo "User specified location: $DownLoads"
echo "troubleshooting end"
} ; trblshtng

# Travelling to target directory
command cd "$DownLoads"

# last sanity check:
pathQ() {
while true; do
    read -p "Is \""$PWD"\" correct path? " yn
    case $yn in
        [Yy]* ) echo "SORTING START!" ; break;;
        [Nn]* ) echo "safely cancelling sorting!" ; exit;;
        * ) echo "Please answer yes or no.";;
    esac
done
} # don't ask if we specified to force it:
[[ -n $JustDoIt ]] || pathQ

## Let's go!

### Folder creation:
if [ ! -d _sorted ]
then  mkdir -p _sorted/{docs,audio,pics,video,packages,roms,misc,homework,personal,memes}
else
echo '"_sorted" folder detected, nice!'
fi

# file count:
echo "There are total of $(command ls | wc -l) files inside downloads folder!"

### moving files with specific {extensions}
### into their appropiate _sorted/folders:
    # Don't moan about no file to move: 2>/dev/null
mv -- *.{pdf,txt,docx,log,org,bat,sh,el} _sorted/docs 2>/dev/null
mv -- *.{wav,ogg,m4a,mp3,flac} _sorted/audio 2>/dev/null
mv -- *.{vgz,vgm,nsf,aac,sf2,mid,spc,aiff} _sorted/audio 2>/dev/null
mv -- *.{gif,bmp,png,tff,jpg,jpeg} _sorted/pics 2>/dev/null
mv -- *.{mov,mp4,m4v,avi,webm,webp} _sorted/video 2>/dev/null
mv -- *.{zip,rar,tar,7z,gz} _sorted/packages 2>/dev/null
mv -- *.{gba,nes,gbc,gb,n64,smc,sfc,md,smd,gcm,nds,wad,3ds,pce,sav,srm,mcd,mcr} _sorted/roms 2>/dev/null
mv -- * _sorted/misc 2>/dev/null

# file count again:
echo "Now file count shows $(command ls | wc -l), happy?"

# coming back to prev dir
cd -

unsetting() { # clean up after script
echo "unsetting variables.."
unset -v DownLoads
unset -v JustDoIt
echo "unsetting complete!"
} ; unsetting
# trblshtng # to check if unsetting worked
exit