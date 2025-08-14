EXE = starlynn

ifeq ($(OS),Windows_NT)
	NAME := $(EXE).exe
else
	NAME := $(EXE)
endif

all:
	cargo rustc --release -- -C target-cpu=native --emit link=$(NAME)
debug:
	cargo rustc -- -C target-cpu=native -C opt-level=3 -C debug-assertions --emit link=$(NAME)
