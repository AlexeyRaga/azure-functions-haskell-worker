# Azure Functions: Haskell Toolset

A CLI utility to create, run and package Azure Functions written in Haskell.

## Usage

### Init new project

```bash
$ mkdir hello
$ cd hello
$ azure-functions-tools init
```

Alternatively:

```bash
$ azure-functions-tools init --project-dir /tmp/hello
```

The `--project-dir` option is useful when working on `azure-functions-tools` because it can be invoked as

```bash
$ cabal run azure-functions-tools -- init --project-dir /tmp/hello
```

### Running projects locally

```bash
$ cd hello
$ azure-functions-tools run
```

Alternatively:

```bash
azure-functions-tools run --project-dir /tmp/hello
```

Or (while working on `azure-functions-tools`):

```bash
cabal run azure-functions-tools -- run --project-dir /tmp/hello
```
