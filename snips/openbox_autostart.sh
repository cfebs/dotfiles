# assuming a xubuntu base

(
    xdg-mime default google-chrome.desktop x-scheme-handler/http;
    xdg-mime default google-chrome.desktop x-scheme-handler/https;
)

# settings?
#(xfsettingsd)&
# for software center etc
(/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1)&
# touchpad
(syndaemon -i 0.5 -d)&
# volume
(xfce-volumed)&
# volumeicon
(volumeicon)&
# bg
(nitrogen --restore)&
# tray
(tint2)&
# conky
(conky -d)&

