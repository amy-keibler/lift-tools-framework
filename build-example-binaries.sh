#!/usr/bin/bash

# have the script fail of any command fails
set -e

# create a folder to contain the output binaries
mkdir -p build-output

# build the ubuntu Dockerfile image that compiles the binaries
docker build -t lift-tools-framework-builder .

# create a temporary container to make the binaries available to "docker cp"
id=$(docker create lift-tools-framework-builder)

# copy the binaries to the host filesystem
docker cp $id:/app/build/. build-output/

# clean up the temporary container
docker rm -v $id
