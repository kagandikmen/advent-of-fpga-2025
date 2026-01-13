DAY ?= day01
BOARD ?= arty-a7-100
EXE := ../_build/default/src/$(DAY)/$(DAY)_arty.exe
TOPV := hardcaml_arty_top.v

$(EXE):
	dune build ../src/$(DAY)/$(DAY)_arty.exe

$(TOPV): $(EXE)
	$< > $@

outputs:
	mkdir -p outputs

outputs/post_synth.dcp: synth.tcl $(TOPV) | outputs
	vivado -nojournal -mode batch -source synth.tcl -tclargs $(BOARD) -log outputs/synth.log

outputs/post_place.dcp: place.tcl outputs/post_synth.dcp | outputs
	vivado -nojournal -mode batch -source place.tcl -log outputs/place.log

outputs/hardcaml_arty_top.bit outputs/post_route.dcp: route.tcl outputs/post_place.dcp | outputs
	vivado -nojournal -mode batch -source route.tcl -log outputs/route.log
	rm -f usage_statistics_webtalk.html usage_statistics_webtalk.xml

clean:
	rm -rf outputs
	rm -f $(TOPV) *.log

clean_all: clean
	rm -rf .Xil/
	rm -f *.txt

