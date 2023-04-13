#!/data/data/com.termux/files/usr/bin/bash
termux-toast "$(ls $HOME/downloads/ -1 | wc -l) files\n in ~/downloads!"
if [[ $(ls $HOME/downloads/ -1 | wc -l) -gt 0 ]]; then
  mv $HOME/downloads/* $HOME/storage/downloads/
fi