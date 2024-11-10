# INFOMPSV-wlp
WLP-based bounded symbolic verification tool for the '24-'25 INFOMPSV course @ UU, by:
- André Agius
- Martijn Voordouw
- Olaf van der Aart

---

## Usage
### Building
Building the tool is easy using the dev container and cabal: simply run `cabal build` from the root of the project. For troubleshooting using a dev container, please refer to the [project template manual](#project-template-manual).

### Usage
The basic format of the command to use the tool is:
```bash
cabal run exe:wlp-verifier -- {PATH} {OPTS}
```
For example, to run the verifier on `examples/array.gcl` with a maximum depth of 12, we use:
```bash
cabal run exe:wlp-verifier -- "./examples/array.gcl" -k 12

# example output:
[INFO] No. paths generated: 1
[INFO] No. branches pruned: 0
[INFO] Total investigated formula size: 41
accept
CPU time:   0.01s
```
To investigate all options, please run:
```bash
cabal run exe:wlp-verifier -- --help
```

### Experiments
The tool also includes a small set of predefined experiments, which can be run using `cabal run exe:wlp-verifier -- --experiments`. The statistics will be outputted to `out.csv` in the root of the project. Note that all other flags are ignored when using this mode. 

## Project template manual

This template (by Tjalle Schoonderwoerd) includes a dev container configuration, as well as the Haskell code for a project with z3 and the GCL parser.  
The dev container provides all necessary dependencies for working on the project: z3 and Haskell tooling.

An example for using z3 is also included. More examples can be found [here](https://github.com/IagoAbal/haskell-z3/tree/master/examples).

### Using the dev container

Prerequisites:
- [Docker](https://www.docker.com/) is installed and running.
- The [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) for [Visual Studio Code (VSCode)](https://code.visualstudio.com/) is installed.

When opening this folder in VSCode, a popup should appear prompting you to reopen the folder in a dev container. If not, this can be done by pressing <kbd>F1</kbd> and finding the command `Dev Containers: Reopen in Container`.  
Now, the container will start to build. The first time, this may take a while.

When the container is finished building, this same folder will be opened inside the container. Now, you can start working on the project like normal.

### Notes on HLS

Whenever the cabal file is changed (`wlp-verifier.cabal` in this template), the project needs to be rebuilt (`cabal build`) for HLS to notice these changes.

Unfortunately, Haskell tooling is always somewhat unstable. This particular version requires `Main.hs` to always be open.  
If the language support is not working properly, try the following:
1. Check if it is still busy setting up. With a cabal project, this may take a bit of time.
2. (Re)open `Main.hs` if you see any errors, or if it is not doing anything at all.
3. If it is still not working, restart HLS. (<kbd>F1</kbd> -> `Haskell: Restart Haskell LSP Server`).
4. If this all fails, leave the dev container and reopen it.
