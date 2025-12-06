MAKEFLAGS += --silent

INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default: install

build:
	dune fmt || true
	dune build

install: build
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	dune clean
	
doc:
	dune build @doc
	open _build/default/_doc/_html/index.html

.PHONY: default build install uninstall reinstall clean