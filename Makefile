.PHONY: clean
clean:
	rm -rf dist-newstyle
	rm -rf verilog
	rm -rf netlist

format:
	stylish-haskell -c ./.stylish-haskell.yaml -r -i src tests

check_format:
	cp ./.stylish-haskell.yaml ./.stylish-haskell-check.yaml
	echo "exit_code: error_on_format" >> ./.stylish-haskell-check.yaml
	stylish-haskell -c ./.stylish-haskell-check.yaml -r src tests

clean_tests:
	rm ./.stylish-haskell-check.yaml

test:
	cabal test

/run/netns/colorlight:
	sudo ip netns add colorlight
	sudo ip link set $(IFNAME) netns colorlight
	sudo ip netns exec colorlight ip addr add 192.168.1.0/24 dev $(IFNAME)
	sudo ip netns exec colorlight ip6tables -A OUTPUT -p icmpv6 --icmpv6-type router-solicitation -j DROP
	sudo ip netns exec colorlight ip link set $(IFNAME) up

.PHONY: namespace
namespace: /run/netns/colorlight

.PHONY: delete_namespace
delete_namespace:
	@if [ -f /run/netns/colorlight ]; then \
		echo "Deleting namespace colorlight"; \
		sudo ip netns delete colorlight; \
	else \
		echo "Namespace colorlight does not exist"; \
	fi

.PHONY: python_test
python_test: /run/netns/colorlight prog
	sudo ip netns exec colorlight \
	sudo "IFNAME=$(IFNAME)" "DEV=$(DEV)" "PATH=$$PATH" "PYTHONPATH=$$PYTHONPATH" python3 -m unittest discover -s python_tests

HASKELL_SOURCES=$(shell find src -type f -iname '*.hs')

# target=Clash.Lattice.ECP5.Colorlight.TopEntity
target=Clash.TinyTapeout.EthernetMac.TopEntity

verilog=verilog/${target}.topEntity/topEntity.v
netlist=netlist/synth.json
pnr=netlist/pnr.cfg
bitstream=netlist/clash-eth.bit

${verilog}: ${HASKELL_SOURCES}
	cabal run clash -- ${target} --verilog -g -fclash-clear

.PHONY: verilog
verilog: $(verilog)

${netlist}: ${verilog}
	mkdir -p netlist
	yosys \
		-m ${YOSYS_ECP5_INFER_OUTREG_LIB} \
		-p "synth_ecp5 -no-rw-check -abc2 -top topEntity" \
		-p "ecp5_infer_bram_outreg" \
		-p "write_json ${netlist}" \
		verilog/${target}.topEntity/*.v

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

netlist_resource=netlist/resource.json
resource_check_pnr=netlist/resource.cfg

.PHONY: resource_check
resource_check: $(verilog)

${netlist_resource}: ${verilog}
	mkdir -p netlist
	yosys \
		-m ${YOSYS_ECP5_INFER_OUTREG_LIB} \
		-p "synth_ecp5 -no-rw-check -abc2 -top Clash_Lattice_ECP5_Colorlight_TopEntity_topEntity_macDepacketizerC" \
		-p "ecp5_infer_bram_outreg" \
		-p "write_json ${netlist_resource}" \
		verilog/${target}.topEntity/*.v

.PHONY: resource_check_pnr
resource_check_pnr: $(resource_check_pnr)

${resource_check_pnr}: ${netlist_resource} pinout.lpf
	nextpnr-ecp5 --json ${netlist_resource} \
		--lpf pinout.lpf \
		--textcfg ${resource_check_pnr} --25k \
		--speed 6 \
		--package CABGA256 \
		--randomize-seed --timing-allow-fail \
		--lpf-allow-unconstrained

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
