#!/usr/bin/env bash

howto() {
    cat >&2 <<"EOM"
Video converter to super-compatible mp4 file!
requires: ffmpeg
$1 - file
$2 - crt - default 21, the higher the lighter!
$3 - fps - default is taken from the video
output will be named "filename"crf"value".mp4
lastly, you will be given filesize of output!
===== INFU =====
EOM
}

# uncomment for verbose script
# set -x

# todo: loopify it to be able to process multiple files
# todo: optional flag to cut short files with size above 8mb
# todo: redo parsing argunents
# otherwise I'm stuck parsing vids like this:
# find . -iname "*.mp4" -exec bash ~/scripts/vid2mp4.sh '{}' 30 30 \;
# where '{}' is $1, and 2 values after are $2 and $3

## Parsing arguments:
# $1 - file
# $2 - crf value (default 21)
# $3 - fps (take from original if none specified)

## No arguments, error out
if [[ "$#" -eq 0 ]] ; then
    howto
    exit 1
fi

# $1 is later specified as $ogname

# check for crf value
if [[ -n $2 && $2 -ge 21 && $2 -le 45 ]] ;then
crf="$2"
    else
echo "crf value incorrect" && crf=21
    fi
# check for fps value
if [[ -n $3 ]] ; then
echo "fps located, assigning.."
# fps="-filter:v fps="$3""
# fps="-r "$3""
printf -v fps '%s' "-r " "$3"
    else
echo "defaulting fps to original video.."
fps=""
    fi

## setting variables
ogname="$1"
location="${1%/*.*}"
ext="${1##*.}"
name=$(basename "$1" ".$ext")
# crf specified above
# fps specified above

trblshtng() { ## troubleshooting stats
echo "troubleshooting start:"
echo "1st. argument is $ogname"
echo "file location is $location"
echo "the extension is $ext"
echo "just filename is $name"
echo "the crf value is $crf"
echo "the fps value is $fps"
echo "troubleshooting end"
} ; trblshtng

mp4() { ## conversion command
ffmpeg -hide_banner -i "$ogname" -c:v libx264 -preset slow -pix_fmt yuv420p -profile:v high -vf "scale='min(1280,iw)':-1" $fps -crf "$crf" "$name"crf"$crf".mp4 && echo "$name converted with success!!"
return
} # for some reason putting $fps in function above
# in "double-quotes" puts 'single quotes' in the function
# so it has to be unquoted

## filesize check of the output:
filesize() {
echo "original video takes $(( $( stat -c %s "$ogname") / 1024 / 1024 )) megabytes"
echo "converted one takes $(( $( stat -c %s "$name"crf"$crf".mp4) / 1024 / 1024 )) megabytes!"
}

## Lets go!

mp4 && echo "" && filesize

unsetting() { # clean up after script
echo "unsetting variables.."
unset -v ogname
unset -v location
unset -v ext
unset -v name
unset -v crf
unset -v fps
echo "unsetting complete!"
} ; unsetting
# trblshtng # to check if unsetting worked
exit

# additional debug/reading:
# command to check for all values:
# ffprobe -v error -show_entries stream -of default=noprint_wrappers=1 $ogname
# command to check for specifically fps:
# ffprobe -v error -show_entries stream=r_frame_rate -of default=noprint_wrappers=1:nokey=1 $ogname
# more details on that here:
# https://write.corbpie.com/getting-video-resolution-bitrate-fps-size-and-more-with-ffprobe/
# resizing/scaling in ffmpeg:
# https://ottverse.com/change-resolution-resize-scale-video-using-ffmpeg/
