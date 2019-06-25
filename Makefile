default: all

MINIFY:=0
ELM_MAKE_FLAGS:=

SOURCES:=$(wildcard src/*.elm) $(wildcard src/*/*.elm) $(wildcard src/*/*/*.elm)
OUTPUT:=public/elm.js

ELM_CANVAS_COPY=public/elm-canvas.js

.PHONY=all
all: $(OUTPUT) $(ELM_CANVAS_COPY)

$(OUTPUT): $(SOURCES)
	@echo "Compiling $@ from $<"
	elm make $< --output $@ $(ELM_MAKE_FLAGS)
	@if [ "$(MINIFY)" = "1" ]; then \
		echo "Minifying..."; \
		uglifyjs "$@" --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output="$@"; \
	fi

node_modules/elm-canvas/elm-canvas.js:
	npm install elm-canvas@latest

$(ELM_CANVAS_COPY): node_modules/elm-canvas/elm-canvas.js
	cp ./node_modules/elm-canvas/elm-canvas.js ./public/

.PHONY=clean
clean:
	rm -f $(ELM_CANVAS_COPY) $(OUTPUT)

.PHONY=dist
dist: MINIFY:=1
dist: ELM_MAKE_FLAGS += --optimize
dist: all

.PHONY=watch
watch:
	@find src -name '*.elm' -or -name '*.js' | entr $(MAKE)

