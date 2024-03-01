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

HASKELL_SOURCES=$(shell find src -type f -iname '*.hs')

verilog=verilog/Clash.Lattice.ECP5.Colorlight.TopEntity.topEntity/topEntity.v
netlist=netlist/synth.json
pnr=netlist/pnr.cfg
bitstream=netlist/clash-eth.bit

${verilog}: ${HASKELL_SOURCES}
	cabal run clash -- Clash.Lattice.ECP5.Colorlight.TopEntity --verilog -fclash-clear

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
