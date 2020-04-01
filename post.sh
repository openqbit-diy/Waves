#!/bin/bash

DOCKER_IMAGE_TAGS=`git tag --points-at HEAD`
sbt "node/assembly"
docker build -f dockerfile.patched.docker -t wavesplatform/wavesnode:${DOCKER_IMAGE_TAGS} .
