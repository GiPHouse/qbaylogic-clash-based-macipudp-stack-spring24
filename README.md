[![](https://github.com/enjoy-digital/liteeth/workflows/ci/badge.svg)](https://github.com/GiPHouse/qbaylogic-clash-based-macipudp-stack-spring24/actions)

# Table of Contents
TODO

# Introduction
Clash Ethernet is a fully configurable Ethernet core written in Clash. 
It aims to become part of the 
[clash-protocols](https://github.com/clash-lang/clash-protocols) library 
which exists to make it easy to develop and use on-chip communication 
protocols, with a focus on protocols in need of bidirectional communication.

This repo implements the following internet protocols:
- Ethernet
- ICMP
- IP
- ARP

Every protocol is fully configurable in data width. This is something that other
libraries, such as [LiteEth](https://github.com/enjoy-digital/liteeth) and
[verilog-ethernet](https://github.com/alexforencich/verilog-ethernet) do not
support.

# Comparison with other implementations

TODO: add speed comparisons for different protocols
| Feature | Clash Ethernet | Verilog | LiteEth |
|-
| Data width (in bytes) | Fully configurable | 1 or 8 | 1, 2, 4 or (partially) 8 |
| protocols | ARP*, ICMP(echo), IP | ARP*, IP, UDP | ARP*, DHCP, ICMP(echo), UDP |
\* Clash Ethernet and Verilog Ethernet have a full ARP table, LiteEth has only 2 registers.

```
verifyChecksum |> depacketizerC const |> verifyLength
```
Source: IpDepacketizer.hs line 30
This verifies the checksum of a packet, then separates the header from the payload and finally verifies the contents of the header.

- clash makes it easy to combine components to make custom stack by using the ```|>``` operator. TODO add example using ```|>```. Verilog and other will require lots of effort to do this.
- Clash Ethernet: RGMII for ECP5, more coming in the future.
- Verilog: multiple for multiple FPGA's
- LiteEth: multiple for multiple FPGA's



<!-- Features Verilog:
 - gigabit, 10G, and 25G packet processing (8 bit and 64 bit datapaths)
 - handling Ethernet frames as well as IP, UDP, and ARP and the components for constructing a complete UDP/IP stack
 - MAC modules for gigabit and 10G/25G, a 10G/25G PCS/PMA PHY module, and a 10G/25G combination MAC/PCS/PMA module
 - various PTP related components for implementing systems that require precise time synchronization
 - full cocotb testbenches that utilize cocotbext-eth

Features LiteEth:
- Configurable MAC (HW or SW interface)
- ARP / ICMP / UDP (HW or SW) / DHCP
- PHY: MII, RMII 100Mbps PHYs. GMII / RGMII / SGMII / 1000BaseX 1Gbps PHYs. SGMII / 2500BaseX 3.125Gbps PHYs.
- Etherbone (Wishbone over UDP: subordinate and manager support), UDP Streaming. -->




# Table comparing resource usage
Perhaps a resource usage comparison using a simple MAC stack for each. This will also show where we stand in terms of logic usage.

# Documentation
How to access the haddock

# How to use as a user
Examples in the examples directory, maybe something on how to setup build environment?

# How to use as a developer

# How to contact maintainers/developers

<!-- omit in toc -->
# Table of Contents
- [Table of Contents](#table-of-contents)
- [Introduction](#introduction)
- [Comparison with other implementations](#comparison-with-other-implementations)
- [Table comparing resource usage](#table-comparing-resource-usage)
- [Documentation](#documentation)
- [How to use as a user](#how-to-use-as-a-user)
- [How to use as a developer](#how-to-use-as-a-developer)
- [How to contact maintainers/developers](#how-to-contact-maintainersdevelopers)
- [Working with this project](#working-with-this-project)
  - [Running Python tests](#running-python-tests)

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

To use Wireshark with the test suite, first manually create the
namespace with `make namespace`, then run Wireshark in the
namspace with `sudo ip netns exec colorlight sudo wireshark`.
The test suite can then be ran as normal.
Alternatively, Python files can be ran manually with
`sudo ip netns exec colorlight sudo ./<filename>`.
