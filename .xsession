if [[ ! -v _XPROFILE_SOURCED ]]; then
  . ~/.xprofile
fi
unset _XPROFILE_SOURCED

# systemctl --user start hm-graphical-session.target

xmonad

systemctl --user stop graphical-session.target
systemctl --user stop graphical-session-pre.target

# Wait until the units actually stop.
while [[ -n "$(systemctl --user --no-legend --state=deactivating list-units)" ]]; do
  sleep 0.5
done
