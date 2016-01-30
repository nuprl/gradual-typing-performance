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
cd /home/artifact
tar -xf popl-2016-artifact.tar
rm popl-2016-artifact.tar
cd popl-2016-artifact

# Copy over the paper
mv paper/paper.pdf /home/artifact/Desktop/is-sound-gradual-typing-dead.pdf
mv run.sh run-all.sh benchmarks paper scribblings tools ~/Desktop
cd ~/Desktop

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
yes | raco pkg install tools/benchmark-util
raco setup -D # avoid huge memory use from doc build

# Create the scribble docs
cd /home/artifact/Desktop/scribblings
make
ln -s /home/artifact/Desktop/scribblings/README/index.html ~/Desktop/README.html
cd ~/Desktop

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

# Center wallpaper and set bg color
echo "xfconf-query -n -t int -c xfce4-desktop -p /backdrop/screen0/monitorVBOX0/workspace0/image-style -s 1" >> ~/.profile
echo "xfconf-query -n -t uint -t uint -t uint -t uint -c xfce4-desktop -p /backdrop/screen0/monitorVBOX0/workspace0/color1 -s 65535 -s 65535 -s 65535 -s 65535" >> ~/.profile

# Cleanup
rm ~/Desktop/racket-6.2-x86_64-linux-ubuntu-precise.sh
rm -r ~/popl-2016-artifact
