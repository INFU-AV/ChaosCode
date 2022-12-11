#!/usr/bin/env bash

cd "$HOME/.termux/"

# This script swaps Termux properties file for 2nd one.
# Needs `termux-reload-settings`
# diff exits with code 0 only if files are same!
# diff -b "$HOME/.termux/termux.properties" "$HOME/.termux/Rtermux.properties" > /dev/null
diff -b termux.properties Rtermux.properties > /dev/null
if [ "$?" = 0 ]; then
  # echo "currently using Rhanded script"
  echo "Setting L-variant:"
  echo "[--->   -       ]"
  cat Ltermux.properties > termux.properties
else
  # echo "currently using Lhanded script"
  echo "Setting R-variant:"
  echo "[       -   <---]"
  cat Rtermux.properties > termux.properties
fi

# I had an idea to make those 2 spare termux properties + this script as self contained, single file, self-editable script - maybe someday!
# cat Ltermux.properties > termux.properties ; printf 'cat Rtermux.properties > termux.properties' > selfmod.sh ;
