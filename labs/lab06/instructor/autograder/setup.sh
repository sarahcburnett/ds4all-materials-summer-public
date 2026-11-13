#!/usr/bin/env bash

export DEBIAN_FRONTEND=noninteractive
apt-get clean
apt-get update
apt-get install -y wget texlive-xetex texlive-fonts-recommended texlive-plain-generic \
    build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev libgit2-dev texlive-lang-chinese
apt-get install -y libnlopt-dev cmake libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    apt-utils libpoppler-cpp-dev libavfilter-dev  libharfbuzz-dev libfribidi-dev imagemagick \
    libmagick++-dev texlive-xetex texlive-fonts-recommended texlive-plain-generic \
    build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev libgit2-dev texlive-lang-chinese \
    libxft-dev

# install pandoc
wget -nv https://github.com/jgm/pandoc/releases/download/3.1.11.1/pandoc-3.1.11.1-1-amd64.deb \
    -O /tmp/pandoc.deb
dpkg -i /tmp/pandoc.deb

# install mamba
if [ $(uname -p) = "arm" ] || [ $(uname -p) = "aarch64" ] ; \
    then wget -nv https://github.com/conda-forge/miniforge/releases/download/25.3.1-0/Miniforge3-Linux-aarch64.sh \
        -O /autograder/source/mamba_install.sh ; \
    else wget -nv https://github.com/conda-forge/miniforge/releases/download/25.3.1-0/Miniforge3-Linux-x86_64.sh \
        -O /autograder/source/mamba_install.sh ; \
fi
chmod +x /autograder/source/mamba_install.sh
/autograder/source/mamba_install.sh -b
echo "export PATH=/root/miniforge3/bin:\$PATH" >> /root/.bashrc

export PATH=/root/miniforge3/bin:$PATH
export TAR="/bin/tar"

# install dependencies with mamba
mamba env create -f /autograder/source/environment.yml
mamba install -n otter-env -c conda-forge nb_conda_kernels
mamba run -n otter-env bash -c "playwright install-deps && playwright install chromium"
mamba run -n otter-env Rscript /autograder/source/requirements.r

# set mamba shell
mamba shell init --shell bash

# Fail the build if the grading environment is broken. Otter 6.1.6's setup.sh has no `set -e`,
# so without these checks a failed install still shows up on Gradescope as a successful build.
if [ ! -d /root/miniforge3/envs/otter-env ]; then
    echo "BUILD CHECK FAILED: otter-env was not created" >&2
    exit 1
fi
mamba run -n otter-env python -c "import otter, rpy2" || {
    echo "BUILD CHECK FAILED: otter or rpy2 cannot be imported in otter-env" >&2
    exit 1
}
mamba run -n otter-env Rscript -e 'pkgs <- c("ottr", "testthat", "digest", "dplyr", "ggplot2"); missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(missing) > 0) { message("BUILD CHECK FAILED: missing R packages: ", paste(missing, collapse = ", ")); quit(status = 1) }; message(R.version.string); for (p in pkgs) message(p, " ", as.character(packageVersion(p)))' || exit 1
echo "BUILD CHECK PASSED"
