# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.188.0/containers/ubuntu/.devcontainer/base.Dockerfile
# Ubuntu 20.04 is focal LTS, 18.04 is bionic LTS

# The default flavor is gitpod, since vscode let us personalize the build ARGs using the devcontainer.json file
# gitpod username is fixed as "gitpod", we can't customize. Also the USER_UID is 33333. May be to not collied with the default user.
# gitpod don't let us to change the default code path outside /workspace, and only saves changes inside this path (recursivelly)
# code-server in the other hand, has its default path on /home/workspace. Maybe I'll change it to /workspace to keep less differences

# details about HOME, .vscode-R is usually stored in user's HOME. Other configurations is useful to be stored on HOME_WORK, so we can
# keep things between sessions.

ARG flavor=gitpod

# ************************* base-image start ******************************

FROM ubuntu:focal AS base

ARG VENDOR=franzbischoff
ARG PROJECT=false.alarm
ARG USERNAME=gitpod
ARG HOME_USER=/home/${USERNAME}
ARG HOME_WORK=/workspace
ARG WORKSPACE_PATH=${HOME_WORK}/${PROJECT}
ARG USER_UID=33333
ARG USER_GID=${USER_UID}
ARG BUILD_NAME="${PROJECT} dev"
ARG BUILD_DESCRIPTION="ready to use environment for this Ph.D. Thesis"
ARG BUILD_VERSION=2
ARG BUILD_DATE="2022-01-21"
ARG BUILD_REPOSITORY="${VENDOR}/${PROJECT}"
ARG CODE_VERSION=gitpod-code

USER root

WORKDIR /home/

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
  git \
  ca-certificates \
  wget \
  curl \
  sudo \
  && rm -rf /var/lib/apt/lists/*

# Creating the user and usergroup
RUN groupadd --gid ${USER_GID} ${USERNAME} \
  && useradd --uid ${USER_UID} --gid ${USERNAME} -m ${USERNAME} \
  && echo ${USERNAME} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${USERNAME} \
  && chmod 0440 /etc/sudoers.d/${USERNAME}

RUN chmod g+rw /home && \
  mkdir -p ${HOME_WORK}

# adding rocker layer
LABEL org.opencontainers.image.licenses="GPL-2.0-or-later" \
  org.opencontainers.image.source="https://github.com/rocker-org/r-ubuntu" \
  org.opencontainers.image.vendor="Rocker Project" \
  org.opencontainers.image.authors="Dirk Eddelbuettel <edd@debian.org>"

## Set a default user. Available via runtime flag `--user docker`
## Add user to 'staff' group, granting them write privileges to /usr/local/lib/R/site.library
## User should also have & own a home directory (for rstudio or linked volumes to work properly).

RUN apt-get update && apt-get install -y --no-install-recommends \
  software-properties-common \
  dirmngr \
  ed \
  less \
  locales \
  gpg-agent \
  fonts-texgyre \
  && rm -rf /var/lib/apt/lists/*

RUN curl -sL -o- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc \
  && add-apt-repository --yes "ppa:marutter/rrutter4.0" \
  && add-apt-repository --yes "ppa:c2d4u.team/c2d4u4.0+" \
  && add-apt-repository --yes "ppa:edd/misc"

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "C.UTF-8 en_US.UTF-8 UTF-8" >> /etc/locale.gen \
  && locale-gen en_US.utf8 \
  && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV R_BASE_VERSION 4.3.0
ENV RENV_PATHS_ROOT "${HOME_WORK}/.cache/R/renv"
ENV RENV_PATHS_LIBRARY_ROOT "${HOME_WORK}/.cache/R/renv/library"
ENV TZ UTC

# ## During the freeze, new (source) packages are in experimental and we place the binaries in our PPA
# RUN echo "deb http://deb.debian.org/debian experimental main" > /etc/apt/sources.list.d/experimental.list \
#   && echo "deb [trusted=yes] https://eddelbuettel.github.io/ppaR400 ./" > /etc/apt/sources.list.d/edd-r4.list

# Install aditional tools
RUN apt-get update && apt-get install -y --no-install-recommends \
  libharfbuzz-dev \
  libfribidi-dev \
  vim \
  libxml2-dev \
  libfontconfig1-dev \
  libcurl4-openssl-dev \
  libmagick++-dev \
  cargo \
  libavfilter-dev \
  libgpgme-dev \
  libssl-dev \
  libzmq3-dev \
  openssh-client \
  mlocate \
  dbus-x11 \
  socat \
  bash-completion \
  bash-builtins \
  && rm -rf /var/lib/apt/lists/*

# Now install R and littler, and create a link for littler in /usr/local/bin
# Default CRAN repo is now set by R itself, and littler knows about it too
# r-cran-docopt is not currently in c2d4u so we install from source
RUN apt-get update && apt-get install -y --no-install-recommends \
  littler \
  apt-transport-https \
  r-cran-littler \
  r-base=${R_BASE_VERSION}-* \
  r-base-dev=${R_BASE_VERSION}-* \
  r-base-core=${R_BASE_VERSION}-* \
  r-recommended=${R_BASE_VERSION}-* \
  && ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r \
  && ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
  && ln -s /usr/lib/R/site-library/littler/examples/installBioc.r /usr/local/bin/installBioc.r \
  && ln -s /usr/lib/R/site-library/littler/examples/installDeps.r /usr/local/bin/installDeps.r \
  && ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
  && ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
  && install.r docopt \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  && rm -rf /var/lib/apt/lists/*

# Install rmd tools
RUN apt-get update && apt-get install -y --no-install-recommends \
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
  && mkdir ${HOME_USER}/.R \
  && echo _R_CHECK_FORCE_SUGGESTS_=FALSE > ${HOME_USER}/.R/check.Renviron \
  && ln -s /usr/lib/R/site-library/littler/examples/render.r /usr/local/bin/render.r

RUN curl -sSL -o pandoc.deb https://github.com/jgm/pandoc/releases/download/2.17.0.1/pandoc-2.17.0.1-1-amd64.deb \
  && dpkg -i pandoc.deb \
  && rm pandoc.deb

# Install tidy stuff
RUN apt-get update && apt-get install -y --no-install-recommends \
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
RUN apt-get update && apt-get install -y --no-install-recommends \
  build-essential \
  protobuf-compiler \
  libprotobuf-dev \
  libprotoc-dev \
  libgit2-dev \
  procps \
  htop \
  gnupg \
  expect \
  graphviz \
  ripgrep \
  pinentry-tty \
  net-tools \
  jq \
  zsh \
  zsh-autosuggestions \
  zsh-syntax-highlighting \
  && rm -rf /var/lib/apt/lists/*

# Install go + protobuf
RUN curl -sL -o ${HOME_USER}/gobuf.tar.gz https://go.dev/dl/go1.17.7.linux-amd64.tar.gz \
  && tar -xf ${HOME_USER}/gobuf.tar.gz -C ${HOME_USER} \
  && rm ${HOME_USER}/gobuf.tar.gz

# adding franz layer
RUN curl -sL -o /usr/bin/hadolint https://github.com/hadolint/hadolint/releases/download/v2.8.0/hadolint-Linux-x86_64 \
  && chmod 755 /usr/bin/hadolint

# Install other packages
RUN install.r targets tarchetypes jsonlite rlang \
  clustermq workflowr R.matlab visNetwork \
  bookdown lubridate kableExtra renv \
  thesisdown here Rcpp RcppParallel matrixprofiler \
  RcppProgress RcppThread codetools signal e1071 \
  lattice MASS Matrix mgcv nlme proffer languageserver

# install dotnet
RUN curl -sSL -o ${HOME_USER}/packages-microsoft-prod.deb "https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/packages-microsoft-prod.deb" \
  && dpkg -i ${HOME_USER}/packages-microsoft-prod.deb \
  && rm ${HOME_USER}/packages-microsoft-prod.deb

RUN curl -sL https://deb.nodesource.com/setup_14.x | bash - \
  && apt-get update && apt-get install -y --no-install-recommends \
  aspnetcore-runtime-3.1 \
  nodejs \
  gcc \
  g++ \
  make \
  && curl -sL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
  && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
  && apt-get update && apt-get install -y --no-install-recommends \
  yarn \
  && rm -rf /var/lib/apt/lists/*

RUN localedef -c -i en_US -f UTF-8 en_US.UTF-8

WORKDIR ${HOME_USER}

RUN git clone --depth 1 https://github.com/nojhan/liquidprompt \
  && echo "# Only load Liquidprompt in interactive shells, not from a script or from scp" >> .bashrc \
  && echo "[[ $- = *i* ]] && source ~/liquidprompt/liquidprompt" >> .bashrc

RUN chown -R ${USERNAME}:${USERNAME} /home
RUN chown -R ${USERNAME}:${USERNAME} ${HOME_WORK}

# cleanunp
RUN apt-get clean autoclean && apt-get autoremove --yes \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf /var/lib/cache/* \
  && rm -rf /var/lib/log/*

RUN groupadd -g 1001 docker \
  && usermod -aG adm,dialout,docker,cdrom,floppy,sudo,audio,dip,video,plugdev,staff ${USERNAME} \
  && usermod --shell /bin/bash ${USERNAME}

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

USER ${USERNAME}

# R environ
ENV PROFFER_PPROF_BIN "${HOME_USER}/go/pkg/tool/linux_amd64/pprof"
ENV PROFFER_GO_BIN "${HOME_USER}/go/bin/go"
ENV PROFFER_GRAPHVIZ_BIN "/usr/bin/dot"
ENV RMARKDOWN_MATHJAX_PATH "/usr/lib/mathjax-27"
ENV VSCODE_WATCHER_DIR "${HOME_USER}/.vscode-R"
ENV RENV_CONFIG_AUTO_SNAPSHOT=true
ENV RENV_CONFIG_CACHE_SYMLINKS=false
ENV RENV_CONFIG_EXPORTED_FUNCTIONS=false
ENV RENV_CONFIG_UPDATES_CHECK=true
ENV RENV_CONFIG_UPDATES_PARALLEL=4

RUN ln -s "${HOME_WORK}/.vscode-remote" "${HOME_USER}/.vscode-remote"

# ************************* base-image end ******************************

# ************************* vscode start ******************************

FROM base AS branch-vscode

ARG USERNAME=coder

RUN echo "This will be a version for vscode"

# Modified from https://github.com/tailscale/codespace/
# Copyright (c) 2021 Tailscale Inc & AUTHORS All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in their LICENSE file.

USER root

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV DEBIAN_FRONTEND noninteractive

# Magic DNS in a container where /etc/resolv.conf is a bind mount needed
# extra support, currently on a development branch.
RUN mkdir -p /home/tailscale /var/run/tailscale /var/cache/tailscale /var/lib/tailscale

WORKDIR /home/tailscale

COPY tailscaled /etc/init.d

RUN chmod 755 /etc/init.d/tailscaled \
  && git clone --depth 1 https://github.com/tailscale/tailscale.git

WORKDIR /home/tailscale/tailscale

ENV GOPATH "${HOME_USER}/go"
ENV PATH ${GOPATH}/bin:${PATH}

RUN go mod download \
  && go install -mod=readonly ./cmd/tailscaled ./cmd/tailscale

RUN mv ${GOPATH}/bin/tailscaled /usr/sbin/tailscaled \
  && mv ${GOPATH}/bin/tailscale /usr/bin/tailscale \
  && rm -rf /home/tailscale

RUN apt-get update && apt-get install -y --no-install-recommends \
  iptables \
  iputils-ping \
  gpg \
  dnsutils \
  && rm -rf /var/lib/apt/lists/*

RUN apt-get clean autoclean && apt-get autoremove --yes \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf /var/lib/cache/* \
  && rm -rf /var/lib/log/*

RUN updatedb

USER ${USERNAME}

ENV NOT_CRAN="true" \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  HOME=${HOME_USER}

# ************************* vscode end ******************************

# ************************* gitpod start ******************************

FROM base AS branch-gitpod

ARG USERNAME=gitpod
ARG HOME_USER=/home/${USERNAME}
ARG HOME_WORK=/workspace

USER root

WORKDIR /home/

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV DEBIAN_FRONTEND noninteractive

# ADD git-hours.tgz ${HOME_WORK}
# ADD mathjax.tgz /usr/lib

RUN mkdir -p ${HOME_USER}/exts \
  # && ln -s "${HOME_WORK}/node_modules/git-hours/src/index.js" /usr/bin/git-hours \
  && curl -fsSL https://pkgs.tailscale.com/stable/ubuntu/focal.gpg | apt-key add - \
  && curl -fsSL https://pkgs.tailscale.com/stable/ubuntu/focal.list | tee /etc/apt/sources.list.d/tailscale.list \
  && apt-get update && apt-get install -y --no-install-recommends tailscale \
  && rm -rf /var/lib/apt/lists/*

RUN apt-get clean autoclean && apt-get autoremove --yes \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf /var/lib/cache/* \
  && rm -rf /var/lib/log/*

RUN chown -R ${USERNAME}:${USERNAME} /home
RUN chown -R ${USERNAME}:${USERNAME} ${HOME_WORK}

RUN updatedb

USER ${USERNAME}

ENV NOT_CRAN="true" \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  HOME=${HOME_USER}

# ************************* gitpod end ******************************

# ************************* after-middle start **********************

FROM branch-${flavor} AS after-branch

RUN echo "done"

CMD ["bash"]

# ************************* after-middle end ***********************
