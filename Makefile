CABAL=cabal
EXECUTABLE_NAME=k

dev: build
	ln -sf $$(cabal exec -- which ${EXECUTABLE_NAME}) ${EXECUTABLE_NAME}

install:
	$(CABAL) install --overwrite-policy=always

build:
	cabal build

clean:
	cabal clean
	rm -f ${EXECUTABLE_NAME}

