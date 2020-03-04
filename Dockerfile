FROM fedora:27

RUN dnf -y update \
    && dnf -y install \
        gcc-gfortran \
        gcc-c++ \
        netcdf-fortran-devel \
        gsl-devel \
        metis-devel \
        lapack-devel \
        openblas-devel \
        cmake \
        git \
        libxml2-devel \
        python \
    && dnf clean all

# Install the ccpp framework
RUN git clone https://github.com/gold2718/ccpp-framework \
    && cd ccpp-framework \
    && git checkout CPF_0.2.008 \
    && mkdir build \
    && cd build \
    && export CC=gcc \
    && export FC=gfortran \
    && export CXX=g++ \
    && cmake .. \
    && make

