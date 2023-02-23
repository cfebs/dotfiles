Some manual setup notes

## i3 xrandr

```
cd ~/.screenlayout
ln -s $LAYOUT ./active.sh
```

## weechat

```
/secure set znc $PASS
```

## `~/.exports.creds`

Export certain credentials for current shell env. Use `exportcreds`

Grab some secrets from keepass and export them.

```
eval "$(keepass-creds ~/Sync/Passwords.kdbx -s hello@cfebs.com@migadu:CRED_MAIL_MIGADU)" || true
```

See: [keepass-creds](bin/keepass-creds]
