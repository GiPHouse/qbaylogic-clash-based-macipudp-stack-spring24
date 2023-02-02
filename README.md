<!-- omit in toc -->
# Clash Ethernet Core
TODO: Describe features

<!-- omit in toc -->
# Table of Contents
- [Working with this project](#working-with-this-project)
- [REPL](#repl)

# Working with this project
It's required to have the [nix package manager](https://nixos.org/download.html) working.

After installing nix you can obtain a dev/build environment with:

```bash
nix-shell
```

This will install all the required dependencies into the current shell.

There is a `Makefile` with the following targets:

- `make clean`: Delete all build directories
- `make format`: Run `stylish-haskell` formatter to format source code
- `make test`: Run the test suite
- `make verilog`: Synthesize verilog code from the Clash TopEntity
- `make netlist`: Synthesize a json netlist
- `make pnr`: Place and route the json netlist
- `make bitstream`: Create a bitstream fromthe place and routed netlist
- `make prog`: SRAM program the FPGA. This means the image is gone after a power
   cycle. This is much faster then flashing.
- `make flash`: Flash the bitstream and reboot FPGA

A REPL can be started with:

```
cabal repl
```
