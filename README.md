[![](https://github.com/enjoy-digital/liteeth/workflows/ci/badge.svg)](https://github.com/GiPHouse/qbaylogic-clash-based-macipudp-stack-spring24/actions)

# Table of Contents
TODO

# Introduction
Clash Ethernet is a fully configurable Ethernet core written in Clash.
It aims to become part of the
[clash-protocols](https://github.com/clash-lang/clash-protocols) library
which exists to make it easy to develop and use on-chip communication
protocols, with a focus on protocols in need of bidirectional
communication. Clash Ethernet will to that the internet protocols.

So far, we have implemented the following internet protocols:
- Ethernet
- ICMP
- IP
- ARP

# Comparison with Other Ethernet Stacks

TODO:
- add speed comparisons for different protocols
- perhaps just write the table out in sentences
| Feature               | Clash Ethernet     | Verilog | LiteEth                  |
|-----------------------|:------------------:|:-------:|:------------------------:|
| Data width (in bytes) | Fully configurable | 1 or 8  | 1, 2, 4 or (partially) 8 |
| Protocols:            |                    |         |                          |
| \_ ARP*               | Yes                | Yes     | Yes                      |
| \_ DHCP               | No                 | No      | Yes                      |
| \_ ICMP(echo)         | Yes                | No      | Yes                      |
| \_ UDP                | No                 | Yes     | Yes                      |

\* Clash Ethernet and Verilog Ethernet have a full ARP table, LiteEth has only 2 registers.

Some of the benefits of using Clash-ethernet are:
- Every protocol is fully configurable in data width. This is
  something that other libraries, such as
  [LiteEth](https://github.com/enjoy-digital/liteeth) and
  [verilog-ethernet](https://github.com/alexforencich/verilog-ethernet)
  do not support.

- Clash makes it very easy to combine two or more components, by using
  the ```|>``` operator, to create a custom stack. For example:

  ```
  verifyChecksum |> depacketizerC const |> verifyLength
  ```
  <!-- Source: IpDepacketizer.hs line 30 (probably not anymore after reorganization)-->

  This verifies the checksum of a packet, then separates the header from
  the payload and finally verifies the contents of the header
  (```verifyLength``` is not the most descriptive name for what is
  does).

  Doing the same in either Verilog or LiteEth will require
  significantly more work.

- Every component in Clash Ethernet is fully tested with random input
  data using
  [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog) and
  [Tasty](https://github.com/UnkindPartition/tasty).

  TODO: check test coverage of alternatives.

Something to consider when choosing to use Clash-ethernet is that, for
now, it only work on an ECP5 with an RGMII chip. However more FPGA's
and chips are coming soon:tm:.



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




# Resource Usage Comparison
Perhaps a resource usage comparison using a simple MAC stack for each.
This will also show where we stand in terms of logic usage.

| Component   |   Clash-ethernet |         LiteEth | Verilog |
|-------------|-----------------:|----------------:|--------:|
| Logic LUTs  | 2448/24288 (10%) | 2421/43848 (5%) |         |
| Carry LUTs  |   44/24288  (0%) |  124/43848 (0%) |         |
| RAM LUTs    |  184/ 3036  (6%) |  348/ 5481 (6%) |         |
| RAMW LUTs   |   92/ 6072  (1%) |  174/10962 (1%) |         |
|-------------|-----------------:|----------------:|--------:|
| Total LUT4s | 2768/24288 (11%) | 3067/43848 (6%) |         |
|-------------|-----------------:|----------------:|--------:|
| Total DFF's |   936/24288 (3%) | 1016/43848 (2%) |         |

The table above shows resource usage of Clash-ethernet compared to
LithEth and Verilog, using a data width of 4 and a simple echo design.
The data shows that Clash-ethernet is about 10% more efficient in
terms of DFF's (D-Flip-Flops).

TODO: add Verilog resource usage.

# Documentation
To generate a local html website of the Clash Ethernet documentation
run the following commands:

TODO: need explaination for nix????

```sh
nix-shell
cabal haddock
```

After ```cabal``` is finished it will print the ```<file-path>``` to
an html file to stdout. Open the file in your browser and use it as
the starting point to the Clash Ethernet documentation.

To get inspired, there are some examples for using Clash Ethernet in the
```examples/``` directory.

# How to use as a user
Examples in the examples directory, maybe something on how to setup build environment?

# How to use as a developer
- install ```nix```, run ```nix-shell```

# How to contact maintainers/developers
If you find any bugs please report them
[here](https://github.com/GiPHouse/qbaylogic-clash-based-macipudp-stack-spring24/issues/).


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
