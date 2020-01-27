FROM archlinux/base:latest

RUN pacman -Sy

# setup yay

# install yay deps
RUN pacman -Syyu git sudo pacman go binutils --needed --noprogressbar --noconfirm

# create the user
RUN useradd -m build

# set the user's password to blank
RUN echo "build:" | chpasswd -e

# install devel packages (without systemd)
RUN pkgs=$(pacman -S base-devel --print-format '%n ');pkgs=${pkgs//systemd/};pkgs=${pkgs//$'\n'/}
RUN pacman -S --needed --noprogressbar --noconfirm $pkgs vi

# give the aur user passwordless sudo powers
RUN echo "build      ALL = NOPASSWD: ALL" >> /etc/sudoers

# use all possible cores for subsequent package builds
RUN sed -i 's,#MAKEFLAGS="-j2",MAKEFLAGS="-j$(nproc)",g' /etc/makepkg.conf

# don't compress the packages built here
RUN sed -i "s,PKGEXT='.pkg.tar.xz',PKGEXT='.pkg.tar',g" /etc/makepkg.conf

# install yay
RUN su build -c 'cd; git clone https://aur.archlinux.org/yay.git'
RUN pacman --noconfirm -S file make gcc
RUN pacman --noconfirm -S fakeroot awk
RUN su build -c 'cd; cd yay; makepkg'
WORKDIR /home/build/yay/
RUN pacman -U *.pkg.tar --noprogressbar --noconfirm
RUN rm -rf /home/build/yay

# do a yay system update
RUN su build -c 'yay -Syyu --noprogressbar --noconfirm --needed'

RUN su build -c 'yay -S --needed --noprogressbar --needed --noconfirm nodejs-spago purescript-bin stack gradle kotlin'

# populate caches to speed up compilation
RUN stack setup
WORKDIR /root
RUN git clone https://gitlab.com/csicar/pskt/ pskt-cache
WORKDIR /root/pskt-cache
RUN make 
