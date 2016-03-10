Package structure
-----------------

This repo is the base of two OPAM packages:

- `topkg`
- `topkg-care`

Both share the same [pkg/pkg.ml](pkg/pkg.ml) and [opam](opam)
file. `topkg` simply builds the `Topkg` library and `topkg-care`
builds the `Topkg_care` library and the `topkg` command line tool. The
distinction is made in the `pkg.ml` file based on the package name
passed to the build instructions. For `topkg` the [opam](opam) file
still need to be massaged by removing the lines commented as
topkg-care only in the `depends:` file.

The reason for this twist is that while both `Topkg` and `Topkg_care`
could be distributed together and `Topkg_care` be simply build
depending on optional dependencies being present, this would have a
fatal flaw: `topkg` cannot be used for any of `Topkg_care`'s
dependencies. Since we want to use `topkg` for these dependencies
aswell, this structure allows to cut the dependency cycle that would
occur.

So if you want to develop `topkg` you should do a:

```
opam pin add -kgit topkg topkg#master
opam pin edit topkg # remove topkg-care deps
opam pin add -kgit topkg-care topkg#master
```

