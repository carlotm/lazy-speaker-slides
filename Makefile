public_dir = $(abspath ./_site)
ELM_OPTS ?= --debug

.PHONY: all clean

all: $(public_dir)/index.html

clean:
	rm -rf $(public_dir)
	mkdir -p $(public_dir)

$(public_dir)/index.html: src/index.html $(public_dir)/app.js $(public_dir)/app.css $(public_dir)/favicon.ico
	minify $< > $@

$(public_dir)/app.js: src/Lss.elm
	elm make $(ELM_OPTS) $< --output /tmp/app.js
	minify /tmp/app.js > $@

$(public_dir)/app.css: src/lss.css
	minify $< > $@

$(public_dir)/favicon.ico: src/favicon.ico
	cp $< $@
