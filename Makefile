#!/usr/bin/make -f

TARGET_DIR := target/site
RESOURCES := $(patsubst resources/%,$(TARGET_DIR)/%,$(shell find resources -type f))
LEIN := lein

.PHONY : all dev publish clean

all : $(TARGET_DIR)/js/app.js $(RESOURCES)

$(TARGET_DIR)/js/app.js : $(shell find src -name "*.cljs")
	$(LEIN) cljsbuild once main

$(RESOURCES) : $(TARGET_DIR)/% : resources/%
	mkdir -p $(dir $@)
	cp $< $@

dev :
	$(LEIN) cljsbuild auto dev

publish :
	bash publish-on-gh-pages.sh $(TARGET_DIR)

clean :
	rm -rf target
