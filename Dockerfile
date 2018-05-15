FROM ubuntu:16.04

# Locales
ENV LANGUAGE=en_US.UTF-8
ENV LANG=en_US.UTF-8
RUN apt-get update && apt-get install -y locales && locale-gen en_US.UTF-8

# Colors and italics for tmux
COPY xterm-256color-italic.terminfo /root
RUN tic /root/xterm-256color-italic.terminfo
ENV TERM=xterm-256color-italic

# Common packages
RUN apt-get update && apt-get install -y \
      build-essential \
      curl \
      git  \
      iputils-ping \
      jq \
      libncurses5-dev \
      libevent-dev \
      net-tools \
      netcat-openbsd \
      rubygems \
      ruby-dev \
      silversearcher-ag \
      socat \
      software-properties-common \
      tmux \
      tzdata \
      wget \
      zsh
RUN bash -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
RUN chsh -s /usr/bin/zsh

# Install docker
RUN apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D &&\
      echo "deb https://apt.dockerproject.org/repo ubuntu-xenial main" > /etc/apt/sources.list.d/docker.list &&\
      apt-get install -y apt-transport-https &&\
      apt-get update &&\
      apt-get install -y docker-engine
RUN  curl -o /usr/local/bin/docker-compose -L "https://github.com/docker/compose/releases/download/1.13.0/docker-compose-$(uname -s)-$(uname -m)" &&\
     chmod +x /usr/local/bin/docker-compose

# Install go
RUN add-apt-repository ppa:longsleep/golang-backports
RUN apt-get update
RUN apt-get install -y golang-1.8-go

# Install Node
RUN apt-get install -y nodejs npm

# Install Haskell
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
RUN apt-get update && apt-get install haskell-stack -y

# # Install Java
# RUN add-apt-repository ppa:openjdk-r/ppa && apt-get update
# RUN apt-get install -y openjdk-9-jdk

RUN apt-get install -y \
      autoconf \
      automake \
      cmake \
      g++ \
      libtool \
      libtool-bin \
      pkg-config \
      python3 \
      python3-pip \
      unzip


# Install Emacs
RUN add-apt-repository ppa:kelleyk/emacs && apt-get update && apt-get install -y emacs25


# Setup User & Dotfiles
RUN useradd -d /home/lester -ms /bin/bash -g root -G sudo -p lester lester
USER lester
WORKDIR /home/lester

RUN git clone https://github.com/lramo062/remote-dev-environment
RUN cd /home/lester/remote-dev-environment && cp -r .emacs .emacs.d .zshrc /home/lester/

USER root
RUN mv /root/.oh-my-zsh /home/lester/
RUN apt-get update && apt-get upgrade

USER lester
ENV USER lester
