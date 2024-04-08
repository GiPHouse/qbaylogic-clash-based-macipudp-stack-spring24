.PHONY: clean
clean:
	rm -rf dist-newstyle
	rm -rf verilog
	rm -rf netlist

format:
	stylish-haskell -c ./.stylish-haskell.yaml -r -i src

check_format:
	cp ./.stylish-haskell.yaml ./.stylish-haskell-check.yaml
	echo "exit_code: error_on_format" >> ./.stylish-haskell-check.yaml
	stylish-haskell -c ./.stylish-haskell-check.yaml -r src

clean_tests:
	rm ./.stylish-haskell-check.yaml

test:
	cabal test

.PHONY: namespace
namespace:
	@if [ ! -f /run/netns/colorlight ]; then \
		echo "Adding $(ifname) to namespace colorlight"; \
		sudo ip netns add colorlight; \
		sudo ip link set $(ifname) netns colorlight; \
		sudo ip netns exec colorlight ip addr add 192.168.1.0/24 dev $(ifname); \
		sudo ip netns exec colorlight ip6tables -A OUTPUT -p icmpv6 --icmpv6-type router-solicitation -j DROP; \
		sudo ip netns exec colorlight ip link set $(ifname) up; \
	else \
		echo "Namespace colorlight already exists"; \
	fi

.PHONY: delete_namespace
delete_namespace:
	@if [ -f /run/netns/colorlight ]; then \
		echo "Deleting namespace colorlight"; \
		sudo ip netns delete colorlight; \
	else \
		echo "Namespace colorlight does not exist"; \
	fi

.PHONY: python_test
python_test: prog
	$(MAKE) namespace ifname=$(ifname)
	sudo ip netns exec colorlight sudo "PATH=$$PATH" "PYTHONPATH=$$PYTHONPATH" python3 -m unittest discover -s python_tests

HASKELL_SOURCES=$(shell find src -type f -iname '*.hs')

verilog=verilog/Clash.Lattice.ECP5.Colorlight.TopEntity.topEntity/topEntity.v
netlist=netlist/synth.json
pnr=netlist/pnr.cfg
bitstream=netlist/clash-eth.bit

${verilog}: ${HASKELL_SOURCES}
	cabal run clash -- Clash.Lattice.ECP5.Colorlight.TopEntity --verilog -g -fclash-clear

.PHONY: verilog
verilog: $(verilog)

${netlist}: ${verilog}
	mkdir -p netlist
	yosys -p "synth_ecp5 -dff -abc2 -top topEntity -json ${netlist};" verilog/Clash.Lattice.ECP5.Colorlight.TopEntity.topEntity/*.v

.PHONY: netlist
netlist: $(netlist)

${pnr}: ${netlist} pinout.lpf
	nextpnr-ecp5 --json ${netlist} \
		--lpf pinout.lpf \
		--textcfg ${pnr} --25k \
		--speed 6 \
		--package CABGA256 \
		--randomize-seed --timing-allow-fail

.PHONY: pnr
pnr: $(pnr)

${bitstream}: ${pnr}
	ecppack ${pnr} --bit ${bitstream} --bootaddr 0

.PHONY: bitstream
bitstream: $(bitstream)

prog: ${bitstream}
	sudo "PATH=$$PATH" env ecpprog -S ${bitstream}

flash: ${bitstream}
	sudo "PATH=$$PATH" env ecpprog -p -a ${bitstream}

int:
	cabal run -- clashi

hoogle:
	hoogle server --local --port 8080
