#!/data/data/com.termux/files/usr/bin/bash
# converts most-recent camera video found in DCIM/Camera
LastModifiedDCIMvideo="$(stat -c '%Y %n' $HOME/storage/dcim/Camera/*.mp4 | sort -k1,1nr | head -1 | awk '{print $2}')"

printf -- "$LastModifiedDCIMvideo\n"
# read -p "Choose CRF value, 00-45: "
# [[ $REPLY -ge 1 && $REPLY -le 45 ]] || exit 1
REPLY=28
if [[ -e $LastModifiedDCIMvideo ]]; then
  bash $HOME/bin/vid2mp4.sh $REPLY $LastModifiedDCIMvideo && \
mv "$(stat -c '%Y %n' $HOME/storage/dcim/Camera/*.mp4 | sort -k1,1nr | head -1 | awk '{print $2}')" $HOME/storage/downloads
fi

