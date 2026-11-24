# Elm 0.18 only ships x86-64 Linux binaries, so the build stage is pinned to
# amd64 (it runs under emulation on ARM machines).
FROM --platform=linux/amd64 debian:bookworm-slim AS elm_build

RUN apt-get update \
    && apt-get install --yes --no-install-recommends wget ca-certificates netbase libgmp10 \
    && rm -rf /var/lib/apt/lists/*

RUN wget -q -O /tmp/elm.tar.gz https://github.com/elm-lang/elm-platform/releases/download/0.18.0-exp/elm-platform-linux-64bit.tar.gz \
    && tar -xzf /tmp/elm.tar.gz -C /usr/local/bin \
    && rm /tmp/elm.tar.gz

WORKDIR /home/sftm

COPY . .

RUN ./build.sh /home/sftm/dist

FROM httpd:2.4

COPY --from=elm_build /home/sftm/dist/ /usr/local/apache2/htdocs/
