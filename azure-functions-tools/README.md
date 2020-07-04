# Azure Functions: Haskell Toolset

A CLI utility to create, run and package Azure Functions written in Haskell.

## Usage

### Creating a new project

```bash
$ mkdir -p /tmp/hello
$ cd /tmp/hello
$ azure-functions-haskell init
```

Many commands of `azure-functions-haskell` support `--project-dir` option.
When `--project-dir` is set, the command is applied to the specified directory instead of the current one.

For example, this command creates a new project in `/tmp/hello`:

```bash
$ azure-functions-haskell init --project-dir /tmp/hello
```

The `--project-dir` option is useful when working on `azure-functions-haskell` because it can be invoked as

```bash
$ cabal run azure-functions-haskell -- init --project-dir /tmp/hello
```

### Adding functions to the project

Assuming that the project `/tmp/hello` exists, an `HTTP` function can be added as:

```
$ cd /tmp/hello
$ azure-functions-haskell new http --name echo
```

This command adds `Functions/Echo.hs` module to the project, and the function can be then implemented in this file.

Other "template" functions (ServiceBus queue, Blob, HTTP, etc.) can ne added in the same way.
See `azure-functions-haskell new --help` to see more options.

### Running projects locally

```bash
$ cd hello
$ azure-functions-haskell run
```

Alternatively:

```bash
azure-functions-haskell run --project-dir /tmp/hello
```

Or (while working on `azure-functions-haskell`):

```bash
cabal run azure-functions-haskell -- run --project-dir /tmp/hello
```

### Building docker containers

**NOTE**: [Docker BuildKit](https://docs.docker.com/develop/develop-images/build_enhancements/) is used in generated `Dockerfile`s. Enable it by setting `DOCKER_BUILDKIT=1` environment variable before executing `docker build`.

`azure-functions-haskell` tool generates `Dockerfile` to pack functions as docker containers.

The container can be built as:

```
$ DOCKER_BUILDKIT=1 docker build . -t <tag>
```

