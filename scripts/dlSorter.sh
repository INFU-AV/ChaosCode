#!/usr/bin/env bash

# making the script case-insensitive:
shopt -s nocaseglob

# here put down path to your downloads folder!
DownLoads=""
# ^  ^ ^ ^ ^ right in those quotes! ^ ^ ^ ^ ^

howto() {
    cat >&2 <<"EOM"
    "Downloads" folder sorter!
upon setting the variable for target folder
script will sort out the content into "_sorted/"
-----
please open the script in editor of your choice
and specify path to your downloads folder
you want to sort out!
-----
See end of the script to see which extensions
get sorted to which folders:
feel free to add your own!
Thanks for help from:
fghsgh, NyaNyaCutie
===== INFU =====
EOM
}

# uncomment for verbose script
# set -x

# Show help when there's any argument:
if [[ "$#" -ne 0 ]] ; then
    howto
    exit 1
fi

# check if folder was specified, exists and is writable:
if [[ -n "$DownLoads" &&  -d "$DownLoads" && -w "$DownLoads" ]]; then
DownLoads=OPDIR
# if doesn't, here's some alternatives:
elif [[ "$OSTYPE" == "linux-android" ]]; then # Termux
echo "Termux detected!"
OPDIR="$HOME/storage/downloads"
elif [[ "$OSTYPE" == "msys" ]]; then 
echo "Windows detected!"
OPDIR="/c/Users/InfuMax2/Downloads"
elif [[ "$OSTYPE" == "linux-gnu" ]]; then 
echo "Linux detected!"
OPDIR="/mnt/c/Users/InfuMax2/Downloads"
else
howto
exit 1
fi

#check if downloads folder exists, again:
if [[ -d "$OPDIR" && -w "$OPDIR" ]]; then
echo "folder accessible!"
else
echo "No valid path recognised!"
echo "please read help file:"
howto
exit 1
fi 

trblshtng() { ## troubleshooting stats
echo "troubleshooting start:"
echo "User specified location: $DownLoads"
echo "Directory parsed to script: $OPDIR"
echo "troubleshooting end"
} ; trblshtng

# Let's go!
cd "$OPDIR"

if [ ! -d _sorted ]
then  mkdir -p _sorted/{docs,audio,pics,video,packages,roms,misc,homework,personal,memes}
else
echo '"_sorted" folder detected, nice!'
fi

echo "There are total of $(command ls | wc -l) files inside downloads folder!"

# moving {extensions} to their _sorted/folders:
mv -- *.{pdf,txt,docx,log,org,bat,sh,el} _sorted/docs
mv -- *.{wav,ogg,m4a,mp3,flac} _sorted/audio
mv -- *.{vgz,vgm,nsf,aac,sf2,mid,spc,aiff} _sorted/audio
mv -- *.{gif,bmp,png,tff,jpg,jpeg} _sorted/pics
mv -- *.{mov,mp4,m4v,avi,webm,webp} _sorted/video
mv -- *.{zip,rar,tar,7z,gz} _sorted/packages
mv -- *.{gba,nes,gbc,gb,n64,smc,sfc,md,smd,gcm,nds,wad,3ds,pce,sav,srm,mcd,mcr} _sorted/roms
mv -- * _sorted/misc

echo "Now file count shows $(command ls | wc -l), happy?"

cd -

unsetting() { # clean up after script
echo "unsetting variables.."
unset -v DownLoads
unset -v OPDIR
echo "unsetting complete!"
} ; unsetting
# trblshtng # to check if unsetting worked
exit