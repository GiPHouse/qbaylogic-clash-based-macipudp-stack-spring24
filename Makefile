clean:
	rm -rf verilog
	rm -rf synthesized

format:
	stylish-haskell -c ./.stylish-haskell.yaml -r -i src

test:
	cabal test

verilog: src
	cabal run clash -- Clash.Lattice.ECP5.Colorlight.TopEntity --verilog

netlist: verilog
	mkdir -p synthesized
	yosys -p "synth_ecp5 -dff -abc2 -top topEntity -json ./synthesized/synth.json;" verilog/Clash.Lattice.ECP5.Colorlight.TopEntity.topEntity/*.v

pnr: synthesized/synth.json pinout.lpf
	nextpnr-ecp5 --json synthesized/synth.json \
		--lpf pinout.lpf \
		--textcfg synthesized/pnr.cfg --25k \
		--speed 6 \
		--package CABGA256 \
		--randomize-seed --timing-allow-fail

bitstream: synthesized/pnr.cfg
	ecppack synthesized/pnr.cfg --bit synthesized/clash-eth.bit --bootaddr 0

prog: synthesized/clash-eth.bit
	sudo ecpprog -S synthesized/clash-eth.bit

flash: synthesized/clash-eth.bit
	sudo ecpprog -p -a synthesized/clash-eth.bit
