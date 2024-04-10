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
- `make bitstream`: Create a bitstream from the place and routed netlist
- `make prog`: SRAM program the FPGA. This means the image is gone after a power
   cycle. This is much faster then flashing.
- `make flash`: Flash the bitstream and reboot FPGA
- `make namespace`: Create a networking namespace "colorlight"
- `make delete_namespace`: Delete the networking namespace "colorlight"
- `make python_test`: Program the FPGA and run the Python test suite within a networking namespace.

A REPL can be started with:

```
cabal repl
```

## Running Python tests
To be able to run `make python_test`, copy `env.sh.default` to
`env.sh` and add the ethernet interface (see `ip a`) and serial
port device (something like `/dev/ttyACM0`, see for example
`python -m serial.tools.list_ports`).
