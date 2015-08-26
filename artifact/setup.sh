#!/usr/bin/env bash

# Make sure there is a Desktop
mkdir -p ~/Desktop

# Installing vagrant keys
mkdir ~/.ssh
chmod 700 ~/.ssh
cd ~/.ssh
wget --no-check-certificate 'https://raw.github.com/mitchellh/vagrant/master/keys/vagrant.pub' -O authorized_keys
chmod 600 ~/.ssh/authorized_keys
chown -R artifact ~/.ssh

# Set up the artifact files
cd ~
tar xvf paper-archive.tar
# FIXME: actually set up the benchmarks from the git repo archive

# Copy over the paper
#cp paper-archive/submissions/camera-ready.pdf ~/Desktop/paper.pdf

# Install Racket
# first download latest Racket
wget http://mirror.racket-lang.org/installers/6.2/racket-6.2-x86_64-linux-ubuntu-precise.sh

# Do a local install. A unix-style install is preferable in some ways, but the
# permissions are a pain when overriding packages
sh racket-6.2-x86_64-linux-ubuntu-precise.sh --in-place --dest ~/racket

# Add racket to the path
export PATH=~/racket/bin:$PATH
echo "export PATH=~/racket/bin:$PATH" >> ~/.bashrc

# Install packages that are needed for the artifact
# FIXME: install packages here
raco setup -D # avoid huge memory use from doc build

# Create the scribble docs
# FIXME: make Scribble docs
#cd paper-archive/artifact
#scribble +m --htmls index.scrbl
#mv index ~/readme-folder
#ln -s ~/readme-folder/index.html ~/Desktop/README.html

# Configure XFCE, instead of directly configuring this put it in the
# .bash_profile because the command won't work without X11 running.
#
# Put in .profile because .xsessionrc isn't run by lightdm sometimes
echo "xfconf-query -c xsettings -p /Net/ThemeName -s Xfce" >> ~/.profile
echo "xfconf-query -c xsettings -p /Net/IconThemeName -s Humanity" >> ~/.profile

# Install an .xsession
echo "source .profile"  > ~/.xsession
echo "startxfce4"      >> ~/.xsession

# Create a desktop shortcut for DrRacket
echo "[Desktop Entry]"             > ~/Desktop/DrRacket.desktop
echo "Version=1.0"                >> ~/Desktop/DrRacket.desktop
echo "Type=Application"           >> ~/Desktop/DrRacket.desktop
echo "Name=DrRacket"              >> ~/Desktop/DrRacket.desktop
echo "Comment="                   >> ~/Desktop/DrRacket.desktop
echo "Exec=/home/artifact/racket/bin/drracket" >> ~/Desktop/DrRacket.desktop
echo "Icon=/home/artifact/racket/share/drracket-exe-icon.png" >> ~/Desktop/DrRacket.desktop
echo "Path="                      >> ~/Desktop/DrRacket.desktop
echo "Terminal=false"             >> ~/Desktop/DrRacket.desktop
echo "StartupNotify=false"        >> ~/Desktop/DrRacket.desktop

# Cleanup
cd ~
#rm -r ~/paper-archive
rm paper-archive.tar
rm racket-6.2-x86_64-linux-ubuntu-precise.sh
