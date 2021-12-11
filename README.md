## Usage

Just use this remote as a git repo in `$HOME`

```
cd $HOME
git init .
git remote add origin git@github.com:cfebs/dotfiles.git
git fetch
git checkout -f main
```

Add new files with `git add -f ...`

> `.gitignore` ignores everything by default, force add needed

## History

- 2012-12-08: dotfiles creation
- 2020-03-12: move to ansible
- 2021-12-11: back to simple git in `$HOME`

## Credits

- https://drewdevault.com/2019/12/30/dotfiles.html
