# Azure Functions Haskell Worker

**[NOTE]** This is a very early work in progress.

Azure Functions Haskell Worker is a __library__ that runs Azure functions inside the worker process.
This means that a "real" functions application (executable) is meant to use the worker library
and run its `main` as `Azure.Functions.Worker.runWorker`.

Worker process provides two main commands:

- [run](src/Azure/Functions/Commands/Run.hs) is a main entry point that starts the worker and funs functions
- [init](src/Azure/Functions/Commands/Init.hs) is a command that allows the worker process to configure itself for the Azure Functions Host. Its main responsibility is to write `worker.config.json`.

