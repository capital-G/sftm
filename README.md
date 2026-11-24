# Sympathy for the Machine

Logic puzzle games: transform a formula into a goal formula, one rule at a time.

Website Link: [https://capital-g.github.io/sftm/](https://capital-g.github.io/sftm/)

Forked from [https://github.com/ezacharias/sftm](https://github.com/ezacharias/sftm) which went offline.

The site serves two editions. The start page lets you choose between them:

| Edition | Directory | Problems | Description |
| --- | --- | --- | --- |
| Logic (classic) | [`classic/`](classic) | 37 | Propositional logic: conjunction, disjunction, implication, equivalence. |
| Algebra | [`algebra/`](algebra) | 55 | *A Tactical Approach to Algebraic Problem Solving*: arithmetic, equations, identities, systems of equations. |

The algebra edition was reconstructed from the compiled version archived on the
Wayback Machine. See [`algebra/README.md`](algebra/README.md) for how this was done
and verified.

## Layout

```
site/           start page for choosing an edition (served at /)
classic/        logic edition (served at /classic/)
  src/          Elm 0.18 source
  glyphs.js     SVG glyphs and port wiring, appended to the compiled Elm
  site/         index.html template, main.css, icon.png
algebra/        algebra edition (served at /algebra/), same structure
build.sh        builds both editions and the start page into dist/
```

Each edition works out the URL path it is served from at runtime, so the built
site can be hosted under any path prefix (for example GitHub Pages' `/sftm/`).
The editions save progress in separate `localStorage` keys, so they don't
interfere with each other.

## Run locally

Assuming you have `docker-compose` installed you can simply run this by executing

```sh
docker-compose up
```

After this you can access the page locally via [http://localhost:8080/](http://localhost:8080/).

Elm 0.18 only ships x86-64 Linux binaries, so on ARM machines (such as Apple
Silicon) the build stage runs under emulation and is slower.

## Build

With `elm-make` 0.18 on the `PATH`:

```sh
./build.sh          # writes the static site to dist/
```

`dist/` can be served by any static web server. Each problem, proof and about
page gets its own `index.html` so that deep links work without server rewrites.

## Contact

* [Edwin Zacharias](mailto:ezacharias@schlussweisen.com)
* [Dennis Scheiba](mailto:mail@dennis-scheiba.com)
