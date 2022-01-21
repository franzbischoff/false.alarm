# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.188.0/containers/ubuntu/.devcontainer/base.Dockerfile

# Ubuntu 20.04 is focal LTS, 18.04 is bionic LTS
FROM ubuntu:focal

ARG USERNAME=coder
ARG USER_UID=1000
ARG USER_GID=$USER_UID
ARG HOME=/home/${USERNAME}
ARG HOME_WORK=/home/workspace
ARG BUILD_NAME="false.alarm dev"
ARG BUILD_DESCRIPTION="ready to use environment for this PHD Thesis"
ARG BUILD_VERSION=2
ARG BUILD_DATE="2022-01-21"
ARG BUILD_REPOSITORY="franzbischoff/false.alarm"

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
RUN groupadd --gid $USER_GID $USERNAME \
  && useradd --uid $USER_UID --gid $USERNAME -m $USERNAME \
  && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
  && chmod 0440 /etc/sudoers.d/$USERNAME

RUN chmod g+rw /home && \
  mkdir -p ${HOME_WORK} && \
  chown -R $USERNAME:$USERNAME ${HOME_WORK}

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
  && add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" \
  && add-apt-repository ppa:c2d4u.team/c2d4u4.0+ \
  && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "C.UTF-8 en_US.UTF-8 UTF-8" >> /etc/locale.gen \
  && locale-gen en_US.utf8 \
  && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV R_BASE_VERSION 4.1.2
ENV RENV_PATHS_ROOT ${HOME_WORK}/.cache/R/renv
## This was not needed before but we need it now
## Otherwise timedatectl will get called which leads to 'no systemd' inside Docker
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
  && mkdir ${HOME}/.R \
  && echo _R_CHECK_FORCE_SUGGESTS_=FALSE > ${HOME}/.R/check.Renviron \
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
  zsh \
  zsh-autosuggestions \
  zsh-syntax-highlighting \
  && rm -rf /var/lib/apt/lists/*

# Install go + protobuf
RUN curl -sL -o ${HOME}/gobuf.tar.gz https://dl.google.com/go/go1.14.linux-amd64.tar.gz \
  && tar -xf ${HOME}/gobuf.tar.gz -C ${HOME} \
  && rm ${HOME}/gobuf.tar.gz

# adding franz layer
RUN curl -sL -o /usr/bin/hadolint https://github.com/hadolint/hadolint/releases/download/v2.8.0/hadolint-Linux-x86_64 \
  && chmod 755 /usr/bin/hadolint

# Install other packages
RUN install.r targets tarchetypes \
  clustermq workflowr R.matlab visNetwork \
  bookdown lubridate kableExtra \
  thesisdown here Rcpp RcppParallel \
  RcppProgress RcppThread codetools \
  lattice MASS Matrix mgcv nlme proffer\
  languageserver

# install dotnet
RUN curl -sSL -o ${HOME}/packages-microsoft-prod.deb "https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/packages-microsoft-prod.deb" \
  && dpkg -i ${HOME}/packages-microsoft-prod.deb \
  && rm ${HOME}/packages-microsoft-prod.deb

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

ADD git-hours.tgz ${HOME_WORK}

RUN ln -s "${HOME_WORK}/node_modules/git-hours/src/index.js" /usr/bin/git-hours \
  && chown -R $USERNAME:$USERNAME ${HOME_WORK}

ADD mathjax.tgz /usr/lib

# gitpod start
RUN mkdir -p ${HOME}/exts
# COPY exts/*.vsix ${HOME}/exts

ENV WORKSPACE_DIRECTORY ${HOME_WORK}

# gitpod end

RUN localedef -c -i en_US -f UTF-8 en_US.UTF-8

RUN chown -R $USERNAME:$USERNAME ${HOME_WORK}

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

USER $USERNAME


# R environ
ENV PROFFER_PPROF_BIN "${HOME}/go/pkg/tool/linux_amd64/pprof"
ENV PROFFER_GO_BIN "${HOME}/go/bin/go"
ENV PROFFER_GRAPHVIZ_BIN "/usr/bin/dot"
ENV RMARKDOWN_MATHJAX_PATH "/usr/lib/mathjax-27"
ENV VSCODE_WATCHER_DIR "${HOME_WORK}/.vscode-R"
ENV RENV_PATHS_ROOT "${HOME_WORK}/.cache/R/renv"
ENV RENV_CONFIG_AUTO_SNAPSHOT=true
ENV RENV_CONFIG_CACHE_SYMLINKS=false
ENV RENV_CONFIG_EXPORTED_FUNCTIONS=false
ENV RENV_CONFIG_UPDATES_CHECK=true
ENV RENV_CONFIG_UPDATES_PARALLEL=4
ENV RENV_DEFAULT_R_LIBS_USER "${HOME_WORK}/R/x86_64-pc-linux-gnu-library/4.1"

ENV NOT_CRAN="true" \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  HOME=${HOME_WORK} \
  EDITOR=code \
  VISUAL=code \
  GIT_EDITOR="code --wait"

WORKDIR ${HOME_WORK}/

CMD ["bash"]




# RUN userdel docker \
#   && useradd -p '' docker \
#   && usermod -aG adm,dialout,cdrom,floppy,sudo,audio,dip,video,plugdev docker \
#   && usermod --shell /bin/bash docker \
#   && usermod --shell /bin/bash root
# COPY .zshrc ${HOME}/.zshrc
# COPY .bashrc ${HOME}/.bashrc
# COPY .profile ${HOME}/.profile
# COPY .zshrc /root/.zshrc
# RUN git clone --depth 1 git://github.com/ohmyzsh/ohmyzsh /root/.oh-my-zsh \
#   && git clone --depth 1 git://github.com/ohmyzsh/ohmyzsh ${HOME}/.oh-my-zsh \

