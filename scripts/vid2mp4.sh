#!/usr/bin/env bash

howto() {
local RE=$(printf '\e[0;31m') # REd
local NC=$(printf '\e[0m'   ) # NoColor
local GR=$(printf '\e[1;32m') # GReen
local YW=$(printf '\e[5;33m') # YelloW
local LI=$(printf '\e[4m') # YelloW
local CA=$(printf '\e[1;36m') # CyAn
    cat >&2 <<EOM
- = ${YW}[${NC}Super-Compatible mp4 Video Converter${YW}]${NC} = - 
${GR}usage${NC}: ${GR}{${NC}number between 19-30${GR}}${NC} ${CA}*.mp4${NC}
${RE}requires${NC}: ffmpeg
${CA}output${NC} will be named "filename"crf${CA}"\$1"${NC}.mp4
as bonus, you will be given filesize of output!
(script will halt on any error!)
${YW}=====ARGUMENTS${NC}:
${GR}\$1${NC}: crt value (default 21)
${GR}\$@${NC}: files to convert
${YW}====="WHAT DOES CRT VALUE MEANS?"${NC}:
The ${YW}higher${NC} the value, the ${YW}slimmer${NC} the video!
  (at the ${RE}cost${NC} of potential ${RE}graphical crustiness${NC})
${GR}Value 21${NC} should look like original video
This script should ${RE}not${NC} damage audio at all*
   *(if it does, pls tell me lol)
${YW}=====${NC}INFU${YW}=====${NC}
EOM
}

# leave on Errors, Unknown variables and failed pipes
set -eo pipefail
# Helpful word-splitting argument-handling
IFS=$'\n\t'
# uncomment for verbose script
# set -x

# todo: optional flag to cut short files with size above 8mb

## No arguments, error out
if [[ "$#" -eq 0 ]] ; then
    howto
    exit 1
fi


main(){
# check for crf value
local crf=${1:-21}
if [[ -n $1 && $1 -ge 21 && $1 -le 45 ]] ;then
    crf="$1"
  else
    echo "crf value incorrect" && crf=21
  fi
shift
# if there's no arguments after crf value, error out lol
if [[ -z $1 ]] ; then
    printf "No videos to process!\n"
    printf "Displaying built-in help...\n\n"
    howto
    exit 2
fi
# trblshtng() { ## troubleshooting stats
# echo "troubleshooting start:"
# echo "1st. argument is $ogname"
# echo "file location is $location"
# echo "the extension is $ext"
# echo "just filename is $name"
# echo "the crf value is $crf"
# echo "the fps value is $fps"
# echo "troubleshooting end"
# } ; trblshtng

## Main functionality

erroroopsie(){
printf -- "\nconversion command has failed! "
printf -- "did you put in correct arguments?\n"
printf -- "FFMPEG should print error above for extra guidance!\n"
exit 2
}

# main file-processing loop!
for vid in "$@"; do
    local location="${vid%/*.*}"
    local ext="${vid##*.}"
    local name=$(basename "$vid" ".$ext")
# if conversion fails, exit the script
    printf -- "Converting [${name}.${ext}] ... "
    ffmpeg -hide_banner -loglevel 16 -i "$vid" -c:v libx264 -preset slow -pix_fmt yuv420p -profile:v high -vf "scale='min(1280,iw)':-1" -crf "$crf" "$name"crf"$crf".mp4 || erroroopsie
printf -- "[DONE!]\n"
# check filesize difference
    printf -- "File [${name}.${ext}] video takes $(( $( stat -c %s "$vid") / 1024 / 1024 )) megabytes\n"
    printf -- "While converted one takes $(( $( stat -c %s "$name"crf"$crf".mp4) / 1024 / 1024 )) megabytes!\n"
    done
printf -- "=====Converted all requested videos!=====\n"
} ; main "$@"

exit # FINAL

# additional debug/reading:
# command to check for all values:
# ffprobe -v error -show_entries stream -of default=noprint_wrappers=1 $ogname
# command to check for specifically fps:
# ffprobe -v error -show_entries stream=r_frame_rate -of default=noprint_wrappers=1:nokey=1 $ogname
# more details on that here:
# https://write.corbpie.com/getting-video-resolution-bitrate-fps-size-and-more-with-ffprobe/
# resizing/scaling in ffmpeg:
# https://ottverse.com/change-resolution-resize-scale-video-using-ffmpeg/
