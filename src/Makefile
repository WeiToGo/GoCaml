# All generated and compiled files should go to $(build_dir)
# The executable is created in the root directory.
executable = ../gocaml
build_dir = _build
compile = ocamlc -I $(build_dir) -c -o


all: directories gocaml

# Create _build directory
.PHONY: directories
directories: 
	mkdir -p _build

# Link and produce executable
gocaml: main.ml $(build_dir)/scan.cmo $(build_dir)/token.cmo $(build_dir)/main.cmo
	ocamlc -o $(executable) $(build_dir)/token.cmo $(build_dir)/scan.cmo $(build_dir)/main.cmo  

# Compile main
$(build_dir)/main.cmo: main.ml
	$(compile) $(build_dir)/main.cmo main.ml

# Compile tokens
$(build_dir)/token.cmo: $(build_dir)/token.ml $(build_dir)/token.cmi
	$(compile) $(build_dir)/token.cmo $(build_dir)/token.ml

# Compile tokens interface
$(build_dir)/token.cmi: $(build_dir)/token.mli
	$(compile) $(build_dir)/token.cmi $(build_dir)/token.mli

# Compile scanner
$(build_dir)/scan.cmo: $(build_dir)/scan.ml $(build_dir)/token.cmi
	$(compile) $(build_dir)/scan.cmo $(build_dir)/scan.ml

# Generate scanner
$(build_dir)/scan.ml: scan.mll $(build_dir)/token.ml
	ocamllex -o $(build_dir)/scan.ml scan.mll

# Generate tokens file
$(build_dir)/token.ml: token.mly
	menhir -b $(build_dir)/token --only-tokens token.mly

# Remove everything in _build
.PHOHY: clean
clean: 
	rm -f $(build_dir)/*
