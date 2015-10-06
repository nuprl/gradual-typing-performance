#!/usr/bin/env bash

# Make sure there is a Desktop
mkdir -p ~/Desktop
rm -f ~/Desktop/*

# Installing vagrant keys
mkdir ~/.ssh
chmod 700 ~/.ssh
cd ~/.ssh
wget --no-check-certificate 'https://raw.github.com/mitchellh/vagrant/master/keys/vagrant.pub' -O authorized_keys
chmod 600 ~/.ssh/authorized_keys
chown -R artifact ~/.ssh

# Set up the artifact files
cd ~/Desktop
wget http://ccs.neu.edu/home/types/v0.0.tar.gz
tar -xzf v0.0.tar.gz
rm v0.0.tar.gz

# TODO Copy over the paper
# cp gradual-typing-performance-0.0/paper/draft.pdf ./is-sound-gradual-typing-dead.pdf

# Install Racket
# first download Racket v6.2
wget http://mirror.racket-lang.org/installers/6.2/racket-6.2-x86_64-linux-ubuntu-precise.sh

# Do a local install. A unix-style install is preferable in some ways, but the
# permissions are a pain when overriding packages
sh racket-6.2-x86_64-linux-ubuntu-precise.sh --in-place --dest ~/racket

# Add racket to the path
export PATH=~/racket/bin:$PATH
echo "export PATH=~/racket/bin:$PATH" >> ~/.bashrc

# Install packages that are needed for the artifact
yes | raco pkg install glob
yes | raco pkg install ./gradual-typing-performance-0.0/tools/benchmark-util
raco setup -D # avoid huge memory use from doc build

# TODO Create the scribble docs
#cd TODO/
#scribble +m --htmls README.scrbl
#mv README ~/Desktop/readme-folder
#ln -s ~/Desktop/readme-folder/index.html ~/Desktop/README.html

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

chmod +x ~/Desktop/DrRacket.desktop

# Cleanup
cd ~/Desktop
rm racket-6.2-x86_64-linux-ubuntu-precise.sh
