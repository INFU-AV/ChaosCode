#!/usr/bin/env bash

# making the script case-insensitive:
shopt -s nocaseglob

howto() { # print out help
local RE=$(printf '\e[0;31m') # REd
local NC=$(printf '\e[0m'   ) # NoColor
local GR=$(printf '\e[1;32m') # GReen
local YW=$(printf '\e[5;33m') # YelloW
local CA=$(printf '\e[1;36m') # CyAn
    cat >&2 <<EOM
- = ${YW}[${NC}"Downloads" Directory Sorter${YW}]${NC} = - 
${GR}usage${NC}: ${GR}"${NC}Path/ToThe/MessyFolder${GR}"${NC}
${RE}requires${NC}: only bash!
${YW}output${NC} will be your files sorted ${GR}by extension${NC} into
multiple folders inside ${RE}"${NC}_sorted${RE}"${NC} located in Messy Folder!
(folders can be found inside "Misc" folder)
${YW}=====ARGUMENTS${NC}:
${GR}\$1${NC}: directory to clean up! ${CA}(script will confirm selection later!)${NC}
${YW}=====[EXPAND ME!]${NC}:
Feel free to expand "dlSorter" by
${CA}adding extensions${NC} you want to sort!
${YW}=====${NC}INFU${YW}=====${NC} ${GR}Thanks fghsgh, NyaNyaCutie for help!${NC}
EOM
}

# uncomment for verbose script
# set -x

## No arguments, error out
if [[ "$#" -eq 0 ]] ; then
    howto
    exit 1
fi

main(){
local DownLoads="$1"
## check if folder was specified, exists and is writable:
if [[ -n "$DownLoads" &&  -d "$DownLoads" && -w "$DownLoads" ]]
then
    echo "Directory accessible!"
else
    echo "No valid path recognised!"
    echo "please read help file:"
    echo ""
    howto
    exit 1
fi 

# Travelling to target directory
command cd "$DownLoads"

# last sanity check:
while true; do
    read -p "Is \""$PWD"\" correct path? " yn
    case $yn in
# this checks only for first letter of response
        [Yy]* ) echo "SORTING START!" ; break;;
        [Nn]* ) echo "safely cancelling sorting!" ; exit;;
        * ) echo "Please answer yes or no.";;
    esac
done

## Let's go!

### Folder creation:
if [ ! -d _sorted ]; then
  mkdir -p _sorted/{docs,audio,pics,video,packages,roms,misc,homework,personal,memes}
# last folders arent used but are there for the user's disposition
  else
  echo '"_sorted" folder detected, nice!'
fi

# file count:
Count() { # https://github.com/dylanaraps/pure-bash-bible
    # Usage: count /path/to/dir/*
    #        count /path/to/dir/*/
    printf '%s\n' "$#"
}
echo "There are total of $(Count *) files inside downloads folder!"
echo "$(Count */) of those are directories!"


### moving files with specific {extensions}
### into their appropiate _sorted/folders:
    # Don't moan about no file to move: 2>/dev/null
mv -- *.{pdf,txt,docx,log,org,bat,sh,el} _sorted/docs 2>/dev/null
mv -- *.{wav,ogg,oga,opus,m4a,mp3,flac} _sorted/audio 2>/dev/null
mv -- *.{vgz,vgm,nsf,aac,sf2,mid,spc,aiff} _sorted/audio 2>/dev/null
mv -- *.{gif,bmp,png,tff,jpg,jpeg} _sorted/pics 2>/dev/null
mv -- *.{mov,mp4,m4v,avi,webm,webp} _sorted/video 2>/dev/null
mv -- *.{zip,rar,tar,7z,gz} _sorted/packages 2>/dev/null
mv -- *.{gba,nes,gbc,gb,n64,smc,sfc,md,smd,gcm,nds,wad,3ds,pce,sav,srm,mcd,mcr} _sorted/roms 2>/dev/null
mv -- * _sorted/misc 2>/dev/null
# file count again:
echo "Now file count shows $(Count *), happy?"
# coming back to prev dir (apparently not needed)
# command cd - >&/dev/null
} ; main "$1"
exit