# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.188.0/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version: bionic, focal
ARG ORIGIN=rocker/r-ubuntu:20.04
FROM ${ORIGIN}

# FROM ubuntu-latest

# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>

## Remain current
## This was not needed before but we need it now
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -qq \
  && apt-get dist-upgrade -y

COPY checkbashisms /usr/local/bin

RUN add-apt-repository ppa:apt-fast/stable && apt-get update && apt-get install -y apt-fast

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
  && mkdir ~/.R \
  && echo _R_CHECK_FORCE_SUGGESTS_=FALSE > ~/.R/check.Renviron \
  && cd /usr/local/bin \
  && ln -s /usr/lib/R/site-library/littler/examples/render.r .

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
  && rm -rf /var/lib/apt/lists/*

# Install aditional tools
RUN apt-fast update && apt-fast install -y --no-install-recommends \
  git \
  curl \
  libgit2-dev \
  libharfbuzz-dev \
  libcurl4-openssl-dev \
  libfribidi-dev \
  libssl-dev \
  libzmq3-dev\
  gpg-agent \
  gpgsm \
  zsh \
  zsh-autosuggestions \
  zsh-syntax-highlighting \
  mlocate \
  && rm -rf /var/lib/apt/lists/*

RUN usermod --shell /bin/zsh docker
RUN usermod --shell /bin/zsh root
COPY .zshrc /home/docker/.zshrc
COPY .zshrc /root/.zshrc
RUN git clone --depth 1 git://github.com/ohmyzsh/ohmyzsh /root/.oh-my-zsh
RUN git clone --depth 1 git://github.com/ohmyzsh/ohmyzsh /home/docker/.oh-my-zsh
RUN chown -R docker:docker /home/docker

WORKDIR /workspaces

# Install other packages
RUN install.r targets tarchetypes \
  clustermq workflowr R.matlab visNetwork \
  thesisdown here Rcpp RcppParallel \
  RcppProgress RcppThread codetools \
  lattice MASS Matrix mgcv nlme \
  languageserver

RUN updatedb

CMD ["zsh"]

ARG BUILD_NAME="false.alarm dev"
ARG BUILD_DESCRIPTION="ready to use environment for this PHD Thesis"
ARG BUILD_VERSION=1
ARG BUILD_DATE

# Labels
LABEL \
  maintainer="Francisco Bischoff <fbischoff@med.up.pt>" \
  org.opencontainers.image.title="${BUILD_NAME}" \
  org.opencontainers.image.description="${BUILD_DESCRIPTION}" \
  org.opencontainers.image.authors="Francisco Bischoff <fbischoff@med.up.pt>" \
  org.opencontainers.image.licenses="CC-BY-NC-SA/4.0" \
  org.opencontainers.image.source="https://github.com/${BUILD_REPOSITORY}" \
  org.opencontainers.image.documentation="https://github.com/${BUILD_REPOSITORY}/blob/main/README.md" \
  org.opencontainers.image.created=${BUILD_DATE} \
  org.opencontainers.image.version=${BUILD_VERSION}
