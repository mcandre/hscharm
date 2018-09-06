# hscharm - a Haskell charm wrapper

# EXAMPLE

```console
$ hellocharm



                        Hello Charm! Press Escape, q, or Control-C to quit.


q
$
```

# DOCUMENTATION

https://hackage.haskell.org/package/hscharm

# RUNTIME REQUIREMENTS

(None)

# BUILDTIME REQUIREMENTS

* [GHC Haskell](http://www.haskell.org/) 8+

# BUILD

```console
$ cabal update
$ cabal install --force-reinstalls --only-dependencies --enable-documentation
$ cabal install --force-reinstalls --only-dependencies --enable-tests
$ shake
```

# LINT

```console
$ shake lint
```

# PUBLISH

```console
$ shake publish
```
