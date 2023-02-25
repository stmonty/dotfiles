#!/bin/sh

if [ -d ~/.emacs.d/ ]
then
    echo ".emacs.d found!"
    echo "Replacing init.el"
    cp ./init.el ~/.emacs.d/
    cp ./macos.el ~/.emacs.d/
    cp ./misc/emacs-blackhole.png ~/.emacs.d/
    echo "Done! Enjoy Emacs"
else
    echo ".emacs.d not found. Make sure Emacs is installed by running 'emacs'."
    echo "Aborting install..."
fi

if [ -d ~/roam/ ]
then
    echo "roam folder found!"

else
    echo "Making roam folder in home directory"
    mkdir ~/roam/
fi
