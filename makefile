all:
	cargo rustc --release -- -C target-cpu=native && \
	copy target\release\starlynn.exe .
