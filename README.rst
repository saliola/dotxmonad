========================
My .xmonad configuration
========================

------------------------------
Installation in two easy steps
------------------------------

Clone repo::

    git clone https://github.com/saliola/dotxmonad.git ~/.xmonad

Recompile and restart xmonad::

    xmonad --recompile
    xmonad --restart


-------------------------------------------------------------
Instructions on configuring xmonad with gnome-panel on Ubuntu
-------------------------------------------------------------

These are instructions to get xmonad working with Gnome, Gnome panel, and dmenu
(as launcher) on ubuntu-13.10.

.. note::

    - Xsession errors are logged to ~/.xsession-errors.
    - You cannot start a panel or launcher from within tmux (perhaps a
      reattached tmux session that was launched in another X session...);
      so use a new terminal instead.

.. warning::

    - I had issues with the system finding the correct xmonad.desktop or
      xmonad.session file (not sure what the problem was, however).
      If there are problems, move the default files provided by Ubuntu out
      of the way.

- install xmonad, gnome-panel and dmenu (for use with Mod-p)::

    apt-get install xmonad
    apt-get install gnome-panel
    apt-get install suckless-tools         # dmenu

- If ``/usr/share/applications/xmonad.desktop`` does not exist, then create it;
  the following is the file that is distributed with the xmonad package::

    [Desktop Entry]
    Type=Application
    Name=Xmonad
    Exec=xmonad
    NoDisplay=true
    X-GNOME-WMName=Xmonad
    X-GNOME-Autostart-Phase=WindowManager
    X-GNOME-Provides=windowmanager
    X-GNOME-Autostart-Notify=true

- create ``/usr/share/xsessions/xmonad.desktop`` (move any existing file with
  this name out of the way)::

    [Desktop Entry]
    Name=Xmonad-Gnome Hybrid of Amazingness
    Comment=Tiling window manager
    TryExec=/usr/bin/gnome-session
    Exec=gnome-session --session=xmonad
    Type=XSession

- create ``/usr/share/gnome-session/sessions/xmonad.session``::

    [GNOME Session]
    Name=Xmonad-Gnome Hybrid of Amazingness
    RequiredComponents=gnome-settings-daemon;gnome-panel;xmonad

- clone/copy/link ``~/.xmonad/xmonad.hs`` into place

- launch gnome-terminal and create a profile with the following
  settings (this is used by the "scratchpad" terminal (Mod-t))::

    General
    > Profile name: scratchpad
    > [Deselect] Show menubar by default in new terminals

    Title and Command
    > Initial title: scratchpad
    > When terminal commands set their own titles: Keep initial title

    Scrolling
    > Scrollbar is: Disabled
    > Scrollback: Unlimited

- prevent nautilus from handling the desktop.

    Execute ``dconf-editor``, then go to::

        org > gnome > desktop > background

    deselect::

        show-desktop-icons

    Possibly, you might have to deselect::

        draw-background

- log out; log in and enjoy!

-----
Notes
-----

- Previously, I had xmonad configured to work with the ``unity-2d-launcher``
  and the ``unity-2d-panel``. I used the following
  ``/usr/share/gnome-session/sessions/xmonad.session`` file::

    [GNOME Session]
    Name=Xmonad-Gnome Hybrid of Amazingness
    RequiredComponents=gnome-settings-daemon;
    RequiredProviders=windowmanager;panel;launcher;
    DefaultProvider-windowmanager=xmonad
    DefaultProvider-panel=unity-2d-panel
    DefaultProvider-launcher=unity-2d-launcher

  However, with Ubuntu 13.04, the commands ``unity-2d-panel`` and
  ``unity-2d-launcher`` are no longer available as Ubuntu migrated from
  unity-2d-panel and unity-2d-launcher to the combined unity interface.

- I haven't looked into this yet, but it could be cool to integrate Ubuntu's
  HUD with XMonad.

**References**

- `Xmonad/Using xmonad in Gnome <http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_Gnome>`_
- `Using Xmonad in Unity 2D <http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_Unity_2D>`_

