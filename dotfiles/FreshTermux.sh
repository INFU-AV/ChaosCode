#!/data/data/com.termux/files/usr/bin/bash

# assumes installed termux-api app
# in the future: provide options, why those particular apps etc
echo " -----INFU-----"
echo " ---DOTFILES---"
echo " --------------"
# echo " ----CHOOSE----"
# echo " -INSTALLATION-" 
# echo "-1)-MINI-------"
# echo "-2)----IT/PC---"
# echo "-3)------AUDIO-"

yes | pkg update ; yes | pkg install exa emacs man ttyd sox git neofetch ack ffmpeg python ncdu p7zip termux-api ; termux-setup-storage
curl https://github.com/INFU-AV/ChaosCode/blob/main/dotfiles/.bashrc?raw=true -o "~.bashrc" 
curl https://github.com/INFU-AV/ChaosCode/blob/main/dotfiles/swap.sh?raw=true -o "~/.termux/swap.sh"
curl https://github.com/INFU-AV/ChaosCode/blob/main/dotfiles/Ltermux.properties?raw=true -o "~/.termux/Ltermux.properties"
curl https://github.com/INFU-AV/ChaosCode/blob/main/dotfiles/Rtermux.properties?raw=true -o "~/.termux/Rtermux.properties"
curl https://github.com/INFU-AV/ChaosCode/blob/main/dotfiles/termux.properties?raw=true -o "~/.termux/termux.properties"
curl https://github.com/INFU-AV/ChaosCode/blob/main/dotfiles/colors.properties?raw=true -o "~/.termux/colors.properties"
curl https://github.com/INFU-AV/ChaosCode/blob/main/dotfiles/font.ttf?raw=true -o "~/.termux/font.ttf"
curl https://github.com/INFU-AV/ChaosCode/blob/main/dotfiles/motd?raw=true -o "$PREFIX/etc/motd"