# Sympathy for the Machine: Algebra

*A Tactical Approach to Algebraic Problem Solving.*

This is the algebra edition of the game. It has 55 problems covering arithmetic,
equations, conditional identities, disjunction and systems of equations. It used
to be online at `https://algebra.sympathyforthemachine.com/`.

## Provenance

The original Elm source of this edition was never published. This directory is
a reconstruction of the compiled site archived by the Wayback Machine on
2019-01-22:

    https://web.archive.org/web/20190122104426/https://algebra.sympathyforthemachine.com/

* `src/` was recovered from the archived `main.js`, which was compiled with Elm
  0.18 and only had its whitespace removed. A decompiler rebuilt each top-level
  definition from the generated JavaScript. 231 definitions compile identically
  to the classic edition, so they use the original hand-written classic source,
  including comments and type annotations. The others were decompiled and given
  inferred type annotations.
* `glyphs.js` is the SVG glyph and port bootstrap script that was appended to
  the archived `main.js`, reformatted.
* `site/` holds the archived `index.html`, `main.css` and `icon.png`, unchanged.

## Verification

The reconstruction was compiled with Elm 0.18 and compared, one definition at a
time, with the archived bundle after normalizing whitespace and compiler-generated
variable numbers. 705 of the 717 top-level definitions compile to the same
JavaScript. The other 12 were reviewed by hand and are equivalent. They differ
only in how they are written: `let ( a, b ) = …` vs `case … of ( a, b ) ->`,
parameter names, an always-true tuple tag test, and the glyph script appended to
`main`.

Local variable names in decompiled definitions are generated from their types
(`t1`, `xs`, `m`, …), because the compiled output does not contain the originals.

## Changes after the reconstruction

To serve this edition next to the classic one, the following was changed
after the archived version (see the git history):

* the app reads a `base` flag (the URL path it is served from) and uses it
  for navigation and routing instead of assuming it lives at `/`,
* the problem list links back to the edition chooser ("All editions"),
* Google Analytics and an unused password gate were removed from `index.html`
  and `glyphs.js`, and `index.html` became a template for the generated route
  pages.

## Build

Use `../build.sh`, which builds both editions.
