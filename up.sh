#!/bin/bash -e

tag="ts-bindgen:1"

image="$(docker images --filter "reference=${tag}" --format "{{.ID}}")"

if [ -z "${image}" ]; then
  docker build -t "${tag}" --file Dockerfile.dev .

  image="$(docker images --filter "reference=${tag}" --format "{{.ID}}")"
fi

port="-p 3000:3000"
if [ "$1"  == "--no-port" ]; then
  port=""
fi

exec docker run --rm -it \
  -v "$(pwd)":"$(pwd)":rw \
  -u "${UID}" \
  -e HOME="/tmp" \
  --entrypoint="/bin/sh" \
  --workdir="$(pwd)" \
  "${image}"
