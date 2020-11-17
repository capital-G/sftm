# Sympathy for the Machine

A logic game website.

Website Link: [https://capital-g.github.io/sftm/](https://capital-g.github.io/sftm/)

There is a bug that changes the URL once the site is visited to an invalid address. PR which fixes this are welcome, see `gh-pages` branch.

Forked from [https://github.com/ezacharias/sftm](https://github.com/ezacharias/sftm) which went offline.

## Run locally

Assuming you have `docker-compose` installed you can simply run this by executing

```sh
docker-compose up
```

After this you can access the page locally via [http://localhost:8080/](http://localhost:8080/).

## Build

```sh
elm-make --output elm.js src/Main.elm
cat elm.js assets/glyphs.js > site/main.js
```

## Contact

* [Edwin Zacharias](mailto:ezacharias@schlussweisen.com)
* [Dennis Scheiba](mailto:mail@dennis-scheiba.com)
