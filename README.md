[![](https://github.com/enjoy-digital/liteeth/workflows/ci/badge.svg)](https://github.com/GiPHouse/qbaylogic-clash-based-macipudp-stack-spring24/actions)


# Clash Ethernet
A fully configurable Ethernet core written in [Clash](https://clash-lang.org/).

# Table of Contents
- [Clash Ethernet](#clash-ethernet)
- [Table of Contents](#table-of-contents)
- [Introduction](#introduction)
- [Comparison with Other Ethernet Stacks](#comparison-with-other-ethernet-stacks)
- [Resource Usage Comparison](#resource-usage-comparison)
- [Documentation](#documentation)
- [How to use as a user](#how-to-use-as-a-user)
- [How to use as a developer](#how-to-use-as-a-developer)
    - [Running Python tests](#running-python-tests)
- [How to contact maintainers/developers](#how-to-contact-maintainersdevelopers)
- [Acknowledgements](#acknowledgements)

# Introduction
Clash Ethernet is a fully configurable Ethernet core written in Clash.
It aims to become part of the
[clash-protocols](https://github.com/clash-lang/clash-protocols) library
which exists to make it easy to develop and use on-chip communication
protocols, with a focus on protocols in need of bidirectional
communication. Clash Ethernet will provide the internet protocols.

So far, Clash Ethernet implements the following internet protocols:
- Ethernet
- ICMP
- IP
- ARP

# Comparison with Other Ethernet Stacks

| Feature               | Clash Ethernet     |      Verilog     | LiteEth                  |
|-----------------------|:------------------:|:----------------:|:------------------------:|
| Data width (in bytes) | :white_check_mark:(Fully configurable) |  :x:(1 or 8)      | :x:(1, 2, 4 or (partially) 8) |
| ARP*                  | :white_check_mark: |:white_check_mark:| :white_check_mark:       |
|  DHCP                 | :x:                | :x:              | :white_check_mark:       |
| ICMP(echo)            | :white_check_mark: | :x:              | :white_check_mark:       |
| UDP                   | :x:                |:white_check_mark:| :white_check_mark:       |

\* Clash Ethernet and Verilog Ethernet have a full ARP table, LiteEth has only 2 registers.

Some of the benefits of using Clash Ethernet are:
- Every protocol is fully configurable in data width. This is something that other libraries, such as [LiteEth](https://github.com/enjoy-digital/liteeth) and [verilog-ethernet](https://github.com/alexforencich/verilog-ethernet) do not support.

- Clash makes it very easy to combine two or more components, by using
  the ```|>``` operator, to create a custom stack. For example:

  ```
  verifyChecksum |> depacketizerC const |> verifyLength
  ```
  <!-- Source: IpDepacketizer.hs line 30 (probably not anymore after reorganization)-->

  This verifies the checksum of a packet, then separates the header from the payload and finally verifies the contents of the header (```verifyLength``` is not the most descriptive name for what it does).
  Doing the same in either Verilog or LiteEth would require
  significantly more work.

- Every component in Clash Ethernet is fully tested with random input data using [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog) and [Tasty](https://github.com/UnkindPartition/tasty).


Something to consider when choosing to use Clash-ethernet is that, for
now, it only work on an ECP5 with an RGMII chip. However more FPGA's
and chips are coming soon™.



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

|     Component   | Clash-ethernet | LiteEth | Verilog |
|-----------------|:--------------:|:-------:|:--------:|
| Logic LUTs      | 2448           | 2421    |         |
| Carry LUTs      | 44             | 124     |         |
| RAM LUTs        | 184            | 348     |         |
| RAMW LUTs       | 92             | 174     |         |
| **Total LUT4s** | 2768           | 3067    |         |
| **Total DFF's** | 936            | 1016    |         |

The table above shows resource usage of Clash Ethernet compared to
LithEth and Verilog, using a data width of 4 and a simple echo design.
The data shows that Clash Ethernet is about 10% more efficient in
terms of DFF's (D-Flip-Flops).

TODO: add Verilog resource usage.

# Documentation
Clash Ethernet uses the [Nix package manager](https://nixos.org/) to setup its build environment. To generate a local html website of the Clash Ethernet documentation
run the following commands:

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
Examples in the examples directory, maybe something on how to setup
build environment?

For more information look at
[Clash-protocols](https://github.com/clash-lang/clash-protocols). This
is the overarching framework of which Clash Ethernet is a part.

# How to use as a developer
The easiest way to setup a development environment to start hacking on
Clash Ethernet is to install [nix](https://nixos.org/) (just the
package manager, not the entire linux distribution).

After you've installed `nix`, clone this repo and run `nix-shell`.

```sh
git clone https://github.com/GiPHouse/qbaylogic-clash-based-macipudp-stack-spring24.git clash-ethernet
cd clash-ethernet
nix-shell
```

`nix-shell` will take about 30 minutes to compile and install
everything, after which you'll have all the tools required to start
hacking. Subsequent invocations of `nix-shell` will only take 5
seconds.

There is a `Makefile` with the following targets:

- `clean`: Delete all build directories
- `format`: Run `stylish-haskell` formatter to format source code
- `test`: Run the test suite
- `verilog`: Synthesize verilog code from the Clash TopEntity
- `netlist`: Synthesize a json netlist
- `pnr`: Place and route the json netlist
- `bitstream`: Create a bitstream from the place and routed
  netlist
- `prog`: SRAM program the FPGA. This means the image is gone
   after a power cycle. This is much faster then flashing.
- `flash`: Flash the bitstream and reboot FPGA
- `namespace`: Create a networking namespace "colorlight"
- `delete_namespace`: Delete the networking namespace
  "colorlight"
- `python_test`: Program the FPGA and run the Python test suite
  within a networking namespace.

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

# How to contact maintainers/developers
If you find any bugs please report them
[here](https://github.com/GiPHouse/qbaylogic-clash-based-macipudp-stack-spring24/issues/).


# Acknowledgements
Thanks to all the members of the QbayLogic team of the Software
Engineering course 2024.
