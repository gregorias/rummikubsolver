#!/usr/bin/env bash
echo "Building Docker images"
docker build --platform=linux/amd64 -t lts-linux-amd64 -f - . < ./build/lts-linux-amd64-Dockerfile && \
docker build --platform=linux/amd64 -t rummikubsolver-linux-amd64 -f - . < ./build/rummikubsolver-linux-amd64-Dockerfile || \
exit 1

TMP=$(mktemp -t foo)
TMP_BASENAME=$(basename "$TMP")
mkdir -p release || exit 1

echo "Extracting the built rummikubsolver package"
docker create --platform=linux/amd64 --name "$TMP_BASENAME" rummikubsolver-linux-amd64 && \
docker cp "$TMP_BASENAME":/root/rummikubsolver/release-linux-amd64/release-linux-amd64.zip release/rummikubsolver-linux-amd64.zip && \
docker rm "$TMP_BASENAME"

rm "$TMP"
