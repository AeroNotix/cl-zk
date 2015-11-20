LISP ?= sbcl
SOURCES := $(wildcard *.lisp)
sbcl_TEST_OPTS=--noinform --disable-debugger --quit --load ./run-tests.lisp
export DESTDIR=$(PWD)/.c_src

.PHONY: test

test:
	@$(LISP) $($(LISP)_TEST_OPTS)

.zookeeper:
	git clone --branch release-3.5.1 --depth 1 https://github.com/apache/zookeeper.git .zookeeper

compile-zk-src: .zookeeper
	cd .zookeeper && \
		ant compile_jute
	cd .zookeeper/src/c/ && \
		autoreconf -if && ./configure && make && make install
