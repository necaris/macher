NPM := npm
EASK := node_modules/.bin/eask

# CLI tools for generating demo videos. Not needed for other package-related operations.
AGG := agg
ASCIINEMA := asciinema
FFMPEG := ffmpeg

# Note this also installs eask dependencies via the package's postinstall script.
$(EASK) .eask: package.json package-lock.json Eask
	"$(NPM)" install
	[ -f "$(EASK)" ] && touch "$(EASK)"

.PHONY: lint
lint: $(EASK) .eask
	$(EASK) lint package

.PHONY: test.%
test.%: $(EASK) .eask
	$(EASK) test ert test/macher-$*-tests.el

.PHONY: test
test: test.unit test.functional test.integration

demo/output/%.cast: demo/demo-%.el macher.el demo/demo-init.el $(EASK) .eask
	mkdir -p "$(@D)"
	$(ASCIINEMA) rec --idle-time-limit 2 --command "$(EASK) emacs -nw -l demo/demo-init.el -l $<" --idle-time-limit=1 --cols=180 --rows=50 --overwrite "$@"

demo/output/%.gif: demo/output/%.cast
	$(AGG) --theme=github-dark "$<" "$@"

demo/output/%.mp4: demo/output/%.gif
# Trim blank/loading screens that show up at the start and end of recordings. These values depend
# somewhat on how fast the system responds to commands, but hopefully these work fine.
	@START_TRIM=0.5; \
	END_TRIM=4; \
	DURATION=$$($(FFMPEG) -y -i "$<" 2>&1 | grep "Duration" | cut -d ' ' -f 4 | sed s/,// | awk -F: '{ print $$1*3600 + $$2*60 + $$3 }'); \
	TRIMMED_DURATION=$$(echo "$$DURATION - $$START_TRIM - $$END_TRIM" | bc); \
	$(FFMPEG) -i "$<" -movflags +faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" -ss $$START_TRIM -t $$TRIMMED_DURATION "$@"
