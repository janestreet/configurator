Configurator
============

Configurator has been integrated into
[dune](https://github.com/ocaml/dune), see [this
documentation](https://dune.readthedocs.io/en/latest/configurator.html)
for more details.

Migrating to `dune.configurator`
--------------------------------

To migrate to the new configurator library shipped with dune, do the
following:

- replace `Configurator` by `Configurator.V1` in your code
- replace `configurator` by `dune.configurator` in your `dune`/`jbuild` file
- remove the `-ocamlc %{ocamlc}` in your `dune`/`jbuild`, it is no longer needed
- remove the `"configurator"` dependency in your opam file
