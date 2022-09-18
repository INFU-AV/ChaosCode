#!/data/data/com.termux/files/usr/bin/bash
cd $HOME/.termux/
# This script swaps Termux properties file for 2nd one. Needs `termux-reload-settings`
# diff exits with code 0 only if files are same!
diff -b termux.properties Rtermux.properties > /dev/null
if [ "$?" = 0 ]; then
# if [ ./termux.properties -ef ./Rtermux.properties ]; then
  echo "currently using Rhanded script"
  echo "Swapping for L-variant..."
  cat Ltermux.properties > termux.properties
else
  echo "currently using Lhanded script"
  echo "Swapping for R-variant..."
  cat Rtermux.properties > termux.properties
fi

# I had an idea to make those 2 spare termux properties + this script as self contained, single file, self-editable script - maybe someday!
# cat Ltermux.properties > termux.properties ; printf 'cat Rtermux.properties > termux.properties' > selfmod.sh ;