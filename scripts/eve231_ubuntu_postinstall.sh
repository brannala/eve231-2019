#!/bin/bash

if [[ $EUID -ne 0 ]]; then
    echo "This script must be run as root" 
    exit 1
else
    #Update and Upgrade
    echo "Installing additional software for EVE 231"
    add-apt-repository ppa:kelleyk/emacs -y
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
    add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
    echo "Updating and Upgrading"
    apt-get update && sudo apt-get upgrade -y
    echo "Installing latest emacs"
    apt install emacs26-nox -y
    echo "Installing latest R"
    sudo apt install r-base
    echo "Installing git"
    sudo apt install git
    echo "Installing GNU typist"
    sudo apt install gtypist
fi
