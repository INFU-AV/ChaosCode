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
cd ~
git clone  https://github.com/INFU-AV/ChaosCode
cd ChaosCode/dotfiles
mv -f .termux ~/.termux
mv -f .bashrc ~/.bashrc
mv -f .emacs.d ~/.emacs.d
cd ~
rm -fr ChaosCode/