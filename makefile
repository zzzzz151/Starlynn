EXE = starlynn
RUSTC_FLAGS = -C target-cpu=native

ifeq ($(OS),Windows_NT)
	NAME := $(EXE).exe
	RUSTC_FLAGS += -C link-args=/STACK:16777216
	RUST_ENV :=
else
	NAME := $(EXE)
	RUST_ENV := RUST_MIN_STACK=16777216
endif

all:
	$(RUST_ENV) cargo rustc --release -- $(RUSTC_FLAGS) --emit link=$(NAME)

debug:
	$(RUST_ENV) cargo rustc -- $(RUSTC_FLAGS) -C opt-level=3 -C debug-assertions --emit link=$(NAME)
