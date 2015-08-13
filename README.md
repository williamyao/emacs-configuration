#.emacs.d

My `.emacs.d` directory, or enough of it to setup a fresh install, at least.

Depends on the [Cask](https://cask.readthedocs.org/) package management system.

##Installing

First install [Cask](https://cask.readthedocs.org/), if you haven't already.

```
cd 
&& git clone https://www.github.com/williamyao/emacs-configuration.git .emacs.d/
&& cd .emacs.d/
&& cask install
```

![Screen shot of my emacs. ](https://cloud.githubusercontent.com/assets/5668019/9239755/6660c946-4124-11e5-878f-db4cd5481dfd.png)


Happy hacking!

##Notes

The init file tries to adjust to where Cask is installed, but it may fail. Edit `init.el` if you get errors about not being able to load `cask.el`.

It also uses `~` as the home directory; this may fail in certain systems. Edit `user-home` in `init.el` if this is the case.

To adjust the font, look in `elisp/general-configuration.el`.
