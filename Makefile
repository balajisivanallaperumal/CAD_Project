BSC_PATHS = -p .:%/Prelude:%/Libraries
TOPFILE = tb_float_neg_neg.bsv
TOPMODULE = mktb_float
.PHONY: default
default: compile link

# ----------------------------------------------------------------
# Bluesim compile/link/simulate

BSIM_DIRS =

.PHONY: compile
compile:
	@echo Compiling for Bluesim ...
	bsc -show-range-conflict -show-schedule -sim -g $(TOPMODULE)  -u $(TOPFILE)
	@echo Compiling for Bluesim finished

.PHONY: link
link:
	@echo Linking for Bluesim ...
	bsc -sim -e mktb_float -o ./out
	@echo Linking for Bluesim finished

.PHONY: simulate
simulate:
	@echo Bluesim simulation ...
	./out -V
	@echo Bluesim simulation finished

# ----------------------------------------------------------------
# Verilog compile/link/sim

.PHONY: verilog
verilog:
	@echo Compiling for Verilog ...
	bsc -u -verilog -elab -keep-fires -aggressive-conditions -no-warn-action-shadowing -g $(TOPMODULE)  $(TOPFILE)
	@echo Compiling for Verilog finished

.PHONY: v_link
v_link:
	@echo Linking for Verilog sim ...
	bsc -verilog -e $(TOPMODULE) -o out_v -vsim iverilog  $(TOPMODULE).v
	@echo Linking for Verilog sim finished

.PHONY: v_simulate
v_simulate:
	@echo Verilog simulation...
	./out_v
	@echo Verilog simulation finished

# ----------------------------------------------------------------

.PHONY: clean
clean:
	rm -f  *.bo *.ba mk* *.cxx *.h *.o 

.PHONY: full_clean
full_clean:
	rm -f  *.bo *.ba mk* *.cxx *.h *.o
	rm -f  out  out.so  out_v *.v
