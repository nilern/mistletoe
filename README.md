# mistletoe

Virtual DOM implementation in cljs.

## Usage

FIXME

## TODO

- [ ] Layout properties with backtracking dependency resolution support for default values that depend on the other
      layout properties in the same dimension (horizontal/vertical).
- [ ] Falling back to DOM measurements when there is insufficient information (e.g. only 0-1 of the x/y layout props are
      specified). This will split the DOM update to chunks and has the potential to be quite slow but there seems to be
      no faster solution in the general case.

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
