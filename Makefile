BUILD_MODE ?= dev
LIBRARY_TYPE ?= relocatable
PROCESSORS ?= 0
BUILD_DIR ?= .
INSTALL_DIR ?= .

ALL_LIBRARY_TYPES = static static-pic relocatable

.PHONY: lib
lib:
	gprbuild -k -P adasat.gpr -p -j$(PROCESSORS) \
		--relocate-build-tree="$(BUILD_DIR)" \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XBUILD_MODE=$(BUILD_MODE)

.PHONY: all-libs
all-libs:
	for kind in $(ALL_LIBRARY_TYPES) ; do \
		gprbuild -k -P adasat.gpr -p -j$(PROCESSORS) \
			--relocate-build-tree="$(BUILD_DIR)" \
			-XLIBRARY_TYPE=$$kind \
			-XBUILD_MODE=$(BUILD_MODE) ; \
	done

.PHONY: install
install:
	for kind in $(ALL_LIBRARY_TYPES) ; do \
		gprinstall -P adasat.gpr -p -f \
			-XLIBRARY_TYPE=$$kind \
			-XBUILD_MODE=$(BUILD_MODE) \
			--relocate-build-tree="$(BUILD_DIR)" \
			--prefix="$(INSTALL_DIR)" \
			--build-name=$$kind \
			--build-var=LIBRARY_TYPE ; \
	done

.PHONY: test
clean:
	gprclean -P adasat.gpr \
		--relocate-build-tree="$(BUILD_DIR)" \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XBUILD_MODE=$(BUILD_MODE)

.PHONY: test
test: lib
	python3 testsuite/testsuite.py

