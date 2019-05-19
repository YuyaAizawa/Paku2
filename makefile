JS = js/Paku2.js js/StageEditor.js

ELMC = elm make



.SUFFIXES: .elm .js

.PHONY: all
all: $(JS)

$(JS):js/%.js:src/%.elm
	@if [ ! -d js ]; \
        then mkdir -p js; \
        fi
	$(ELMC) $< --output $@ --optimize

.PHONY: clean
clean:
	$(RM) $(JS)

src/Paku2.elm: src/Stage.elm src/Ports.elm src/Direction.elm
src/Stage.elm: src/Object.elm src/Direction.elm
src/Object.elm: src/Direction.elm