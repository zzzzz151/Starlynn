ifeq ($(OS),Windows_NT)
	NAME := Starlynn.exe
else
	NAME := starlynn
endif

all:
	cargo rustc --release -- -C target-cpu=native --emit link=$(NAME)
