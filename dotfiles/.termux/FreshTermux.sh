#!/data/data/com.termux/files/usr/bin/bash

# cd $HOME ; rm .emacs.d/custom.el ; 7z a Termuxbackup.7z .emacs.d/*.el .config/aliases.sh .termux/* .bashrc && echo "backup complete!" 

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

yes | pkg update && yes | pkg install exa emacs man ripgrep ttyd sox git neofetch miniserve ffmpeg python ncdu p7zip termux-api && termux-setup-storage
cd ~
git clone  https://github.com/INFU-AV/ChaosCode
unfinished yet
# cd ChaosCode/dotfiles
# mv -f .termux ~
# mv -f .bashrc ~
# mv -f .emacs.d ~
# mv -f motd $PREFIX/etc/motd
# cd ~
# rm -fr ChaosCode/
