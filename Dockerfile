FROM debian:buster AS ELM_BUILD

WORKDIR /home/sftm

RUN apt-get update && apt-get install --yes wget netbase

RUN cd /tmp && wget https://dl.bintray.com/elmlang/elm-platform/0.18.0/linux-x64.tar.gz && tar -xzf linux-x64.tar.gz && cp dist_binaries/* /usr/local/bin

RUN cd /home/sftm && rm -r /tmp/*

COPY . .

RUN elm-make --yes --output elm.js src/Main.elm

RUN cat elm.js assets/glyphs.js > site/main.js

FROM httpd:2.4

COPY --from=ELM_BUILD /home/sftm/site /usr/local/apache2/htdocs/
