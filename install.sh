#!/bin/sh

if [ -d ~/.emacs.d/ ]
then
    echo ".emacs.d found!"
    echo "Replacing init.el "
    cp ./init.el ~/.emacs.d/
    cp ./macos.el ~/.emacs.d/
    echo "Done! Enjoy Emacs"
else
    echo ".emacs.d not found. Make sure Emacs is installed by running 'emacs'."
    echo "Aborting install..."
fi
