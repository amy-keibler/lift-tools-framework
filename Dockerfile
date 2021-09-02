# Use the same Ubuntu version that Lift's analysis tools use for compatibility
FROM ubuntu:20.04

# Install all of the dependencies needed to compile Haskell projects (ghc included to pull in all of the required libraries like libgmp-dev)
RUN apt-get update && apt-get install -y \
        build-essential \
        curl \
        ghc

# Install ghcup to get access to a specific version of GHC
RUN curl --output /tmp/ghcup https://downloads.haskell.org/~ghcup/0.1.16.2/x86_64-linux-ghcup-0.1.16.2
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"
RUN chmod u+x /tmp/ghcup

# Install the compiler toolchain we need for our tools
RUN /tmp/ghcup install ghc 8.10.5
RUN /tmp/ghcup set ghc 8.10.5
RUN /tmp/ghcup install cabal

# Update cabal's list of dependencies to enable downloading
RUN cabal v2-update

# Set up the "app" folder with all of the required code
WORKDIR /app/
RUN mkdir build
COPY . .

# build and install the executables to the /app/build/ folder ("copy" as opposed to creating symlinks)
RUN cabal v2-build
RUN cabal v2-install --overwrite-policy=always --install-method=copy --installdir=/app/build/


