# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.188.0/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version: bionic, focal
FROM rocker/r-ubuntu:20.04
ARG BUILD_NAME="false.alarm dev"
ARG BUILD_DESCRIPTION="ready to use environment for this PHD Thesis"
ARG BUILD_VERSION=1
ARG BUILD_DATE="2021-11-10"
ARG BUILD_REPOSITORY="franzbischoff/false.alarm"
ARG HOME="/home/docker"

# FROM ubuntu-latest

# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>

## Remain current
## This was not needed before but we need it now
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -qq \
  && apt-get dist-upgrade -y \
  && rm -rf /var/lib/apt/lists/*

COPY checkbashisms /usr/local/bin

RUN add-apt-repository ppa:apt-fast/stable && apt-get update && apt-get install -y --no-install-recommends apt-fast \
  && rm -rf /var/lib/apt/lists/*

# Install rmd tools
RUN apt-fast update && apt-fast install -y --no-install-recommends \
  ghostscript \
  lmodern \
  pandoc-citeproc \
  qpdf \
  r-cran-formatr \
  r-cran-ggplot2 \
  r-cran-knitr \
  r-cran-rmarkdown \
  r-cran-runit \
  r-cran-testthat \
  texinfo \
  texlive-fonts-extra \
  texlive-fonts-recommended \
  texlive-latex-extra \
  texlive-latex-recommended \
  texlive-luatex \
  texlive-plain-generic \
  texlive-science \
  texlive-xetex \
  && rm -rf /var/lib/apt/lists/* \
  && install.r binb linl pinp tint \
  && mkdir ${HOME}/.R \
  && echo _R_CHECK_FORCE_SUGGESTS_=FALSE > ${HOME}/.R/check.Renviron \
  && ln -s /usr/lib/R/site-library/littler/examples/render.r /usr/local/bin/render.r

# Install tidy stuff
RUN apt-fast update && apt-fast install -y --no-install-recommends \
  r-cran-tidyverse \
  r-cran-bitops \
  r-cran-sourcetools \
  r-cran-xtable \
  r-cran-checkmate \
  r-cran-plotly \
  r-cran-bookdown \
  r-cran-future \
  r-cran-stringi \
  r-cran-shiny \
  r-cran-openssl \
  r-cran-remotes \
  r-cran-devtools \
  r-cran-pkgbuild \
  r-cran-roxygen2 \
  r-cran-zoo \
  r-cran-codetools \
  r-cran-lattice \
  r-cran-mass \
  r-cran-matrix \
  r-cran-mgcv \
  r-cran-nlme \
  && rm -rf /var/lib/apt/lists/*

# Install some dev stuffs
RUN apt-fast update && apt-fast install -y --no-install-recommends \
  build-essential \
  r-base-dev \
  r-recommended \
  r-cran-rcpp \
  protobuf-compiler \
  libprotobuf-dev \
  libprotoc-dev \
  graphviz \
  && rm -rf /var/lib/apt/lists/*

# Install aditional tools
RUN apt-fast update && apt-fast install -y --no-install-recommends \
  git \
  curl \
  libgit2-dev \
  libgpgme-dev \
  libharfbuzz-dev \
  libcurl4-openssl-dev \
  libfribidi-dev \
  libssl-dev \
  libzmq3-dev\
  gnupg \
  pinentry-tty \
  procps \
  htop \
  net-tools \
  gpg-agent \
  expect \
  vim \
  zsh \
  zsh-autosuggestions \
  zsh-syntax-highlighting \
  mlocate \
  sudo \
  && rm -rf /var/lib/apt/lists/*

# Install go + protobuf
RUN curl -o ${HOME}/gobuf.tar.gz https://dl.google.com/go/go1.14.linux-amd64.tar.gz \
  && tar -xf ${HOME}/gobuf.tar.gz -C ${HOME} \
  && rm ${HOME}/gobuf.tar.gz

RUN userdel docker \
  && useradd -p '' docker \
  && usermod -aG adm,dialout,cdrom,floppy,sudo,audio,dip,video,plugdev docker \
  && usermod --shell /bin/bash docker \
  && usermod --shell /bin/bash root
COPY https://github.com/hadolint/hadolint/releases/download/v2.8.0/hadolint-Linux-x86_64 /usr/bin/hadolint
COPY .zshrc ${HOME}/.zshrc
COPY .bashrc ${HOME}/.bashrc
COPY .profile ${HOME}/.profile
COPY .zshrc /root/.zshrc
RUN git clone --depth 1 git://github.com/ohmyzsh/ohmyzsh /root/.oh-my-zsh \
  && git clone --depth 1 git://github.com/ohmyzsh/ohmyzsh ${HOME}/.oh-my-zsh \
  && chmod 755 /usr/bin/hadolint

# Install other packages
RUN install.r targets tarchetypes \
  clustermq workflowr R.matlab visNetwork \
  thesisdown here Rcpp RcppParallel \
  RcppProgress RcppThread codetools \
  lattice MASS Matrix mgcv nlme proffer\
  languageserver

# fill next stuff here, avoid adding slow things above this line

# Install aditional tools
RUN apt-fast update && apt-fast install -y --no-install-recommends \
  libxml2-dev \
  libfontconfig1-dev \
  libmagick++-dev \
  cargo \
  libavfilter-dev \
  openssh-client \
  socat \
  bash-completion \
  bash-builtins \
  && rm -rf /var/lib/apt/lists/*


# this is the last section, avoid adding slow things below this line

# gitpod start
RUN mkdir ${HOME}/exts
# COPY exts/*.vsix ${HOME}/exts

ENV WORKSPACE_DIRECTORY "/workspace"
ENV NOT_CRAN "true"
ENV PROFFER_PPROF_BIN "~/go/pkg/tool/linux_amd64/pprof"
ENV PROFFER_GO_BIN "~/go/bin/go"
ENV PROFFER_GRAPHVIZ_BIN "/usr/bin/dot"
# gitpod end

RUN localedef -c -i en_US -f UTF-8 en_US.UTF-8

RUN chown -R docker:docker ${HOME}

# cleanunp
RUN apt-get clean autoclean && apt-get autoremove --yes \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf /var/lib/cache/* \
  && rm -rf /var/lib/log/*

RUN updatedb

# Labels
LABEL \
  org.opencontainers.image.title="${BUILD_NAME}" \
  org.opencontainers.image.description="${BUILD_DESCRIPTION}" \
  org.opencontainers.image.authors="Francisco Bischoff <fbischoff@med.up.pt>" \
  org.opencontainers.image.licenses="CC-BY-NC-SA/4.0" \
  org.opencontainers.image.source="https://github.com/${BUILD_REPOSITORY}" \
  org.opencontainers.image.documentation="https://github.com/${BUILD_REPOSITORY}/blob/main/README.md" \
  org.opencontainers.image.created=${BUILD_DATE} \
  org.opencontainers.image.version=${BUILD_VERSION}

USER docker
WORKDIR /workspace

CMD ["zsh"]


