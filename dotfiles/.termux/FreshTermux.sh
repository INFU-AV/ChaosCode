#!/data/data/com.termux/files/usr/bin/bash

# Script assumes you got installed:
# -termux-api
# -p7zip
# -git
# in the future: provide options, why those particular apps etc
#

b(){ # needs p7zip & termux-setup-storage:
cd $HOME ; rm .emacs.d/custom.el ; tar --create --verbose --file - .emacs.d/*.el .config/aliases.sh .termux/ .bashrc xinfu bin .shortcuts .ssh | 7z a -si -m0=lzma2 -mx=3 termuxbackup.tar.7z && { echo "backup complete!" ; cp termuxbackup.tar.7z $HOME/storage/downloads ;} && echo "Archive copied to Android's downloads folder!"
}
i() { # Initial personal setup:
termux-setup-storage & yes | pkg update && yes | pkg install p7zip emacs man ripgrep ttyd hunspell sox git jq ffmpeg python python-pip ncdu openssh fdupes wget2 termux-api ; pip install yt-dlp
[[ -e $HOME/.lesshst ]]      && rm -- $HOME/.lesshst
[[ -e $PREFIX/etc/motd ]]    && rm -- $PREFIX/etc/motd*
[[ -e $HOME/.bash_history ]] && rm -- $HOME/.bash_history
}

x(){ # unpack previously made termuxbackup 
cd $HOME ; cp $HOME/storage/downloads/termuxbackup.tar.7z . && { 7z x termuxbackup.tar.7z ; yes Y | 7z x termuxbackup.tar ; rm termuxbackup.tar* ;}
}
ChCd(){ # needs git
cd $HOME
git clone https://github.com/INFU-AV/ChaosCode
exit # unfinished yet
cd ChaosCode/dotfiles
mv -f .termux ~
mv -f .bashrc ~
mv -f .emacs.d ~
cd ~
rm -fr ChaosCode/
}

# works by providing name of functions as arguments
for i in "$@"; do
  "$i" || { echo "one of actions has failed! Please inspect what's going on" ; exit 2 ; }
done