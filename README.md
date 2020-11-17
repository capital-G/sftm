# Sympathy for the Machine

A logic game website.

Forked from [https://github.com/ezacharias/sftm](https://github.com/ezacharias/sftm) which went offline.

## Run

Assuming you have `docker-compose` installed you can simply run this by executing

```sh
docker-compose up
```

## Build

```sh
elm-make --output elm.js src/Main.elm
cat elm.js assets/glyphs.js > site/main.js
```

## Contact

* [Edwin Zacharias](mailto:ezacharias@schlussweisen.com)
* [Dennis Scheiba](mailto:mail@dennis-scheiba.com)
