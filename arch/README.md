## Notes

- To fix startx, early driver KMS defined in /etc/mkinitcpio.conf. Might need to change if using different driver.
    - requires `mkinitcpio -P` after change

- Noto emoji
    - See `home/.config/fontconfig/fonts.conf` idea is to always prepend noto for common families and then include noto emoji in the fallback list
    - Fontmanager gui creates blacklists in `~/.config/` ! I blacklisted color emoji font when I was testing
