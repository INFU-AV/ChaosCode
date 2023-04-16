#!/usr/bin/bash
howto() {
local RE=$(printf '\e[0;31m') # REd
local NC=$(printf '\e[0m'   ) # NoColor
local GR=$(printf '\e[1;32m') # GReen
local YW=$(printf '\e[5;33m') # YelloW
local CA=$(printf '\e[1;36m') # CyAn
    cat >&2 <<EOM
- = ${YW}[${NC}Audio-Video Conversion Kit ${YW}]${NC} = - 
${GR}usage${NC}: AVkit.sh ${GR}"${NC}mediafiles.*${GR}"${NC}
${RE}requires${NC}: ffmpeg
${YW}output${NC} Depending on the option you'll choose, this script will
${GR}convert all files${NC} you pointed out to it into format you want!
  No more trying to remember what was this ffmpeg command,
  and then giving up, researching through stackoverflow again...
${YW}=====ARGUMENTS${NC}:
${GR}\$*${NC}: Audio/Video files to convert!
${YW}=====[INTERACTIVE SCRIPT!]${NC}:
${CA}(You'll need to choose what you want to do with the files)${NC}
${YW}=====${NC}INFU${YW}=====${NC}
EOM
exit 2
}

## If there's no arguments, error out
if [[ "$#" -eq 0 ]] ; then
    howto
    exit 3
fi

# some comments are left there for new-to-scripting users!

any2mp3(){
  ffmpeg -hide_banner -i "$i" -map 0:a -f mp3 -b:a 256k "${i%.*}.mp3"
  #     input^    output reusing same variable^ without old extension!
exit ; }
any2opusRenamed2ogg(){
  # We reuse that variable for output too, just removing old extension
  ffmpeg -hide_banner -i "$i" -map 0:a -c:a libopus -b:a 140k "${i%.*}.ogg"
  # only 1st audio stream^  ^codec       ^bitrate  
  exit ; }
any2wav(){
  # wav files will be exactly 16 bit, mono, with samplerate 44100
  ffmpeg -hide_banner -i "$i" -map 0:a -c:a pcm_s16le -ac 1 -ar 44100 "${i%.*}.wav" 
  exit ; }
audioextraction() { # Extract exact audio from the video file
  # we check audio codec beforehand
  local OutExt="$(ffprobe -v error -select_streams a -show_entries stream=codec_name -of default=noprint_wrappers=1:nokey=1 "$i")"
  ffmpeg -hide_banner -i "$i" -vn -acodec copy "${i%.*}"-extracted."$OutExt"
  exit ; }
Cancel(){
  printf -- "Cancelling..\n"
  sleep 0.4
  exit 3
} 

#CUTTING(){ # NOT YET FINISHED
#  ffmpeg -hide_banner -ss "$start" -i "$i" -t "$dur" -c copy "${i%.*}-${dur}.${i##*.}"
#  exit ; } # NOT YET USED

get_functions() { # Collects all functions in this script
  # https://github.com/dylanaraps/pure-bash-bible
  IFS=$'\n' read -d "" -ra functions < <(declare -F)
  printf '%s\n' "${functions[@]//declare -f }"
}
# get all functions together in array
all_the_functions=("$(get_functions)")
# well, all except that one:
all_the_functions=("${all_the_functions[@]/get_functions}")

main(){
  printf -- "choose which function to run and process arguments!\n"
  PS3="Select an option:"
  select operation in ${all_the_functions[@]}; do
    for  i in "$@" ; do $operation ; done ; done
} ; main "$@"
### RESOURCES:
## Good commands mostly from here:
# https://gist.github.com/protrolium/e0dbd4bb0f1a396fcb55
##  why "-vn" is bad and "-map 0:a" is better:
# https://stackoverflow.com/questions/9913032/how-can-i-extract-audio-from-video-with-ffmpeg
## Pure-Bash-Bible providing god powers to deal with bash!
# https://github.com/dylanaraps/pure-bash-bible

# https://gist.github.com/steven2358/ba153c642fe2bb1e47485962df07c730

# I wanna make normalizing function out of this:
# local Normalization="-filter:a \"loudnorm=I=-15:LRA=15\""
