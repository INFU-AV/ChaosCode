#!/data/data/com.termux/files/usr/bin/bash
TheIP="$(ifconfig 2>/dev/null | grep 'inet 192.168' | head -1 | awk '{print $2}')"
termux-toast -b black -g top "$TheIP"
termux-clipboard-set "$TheIP"
