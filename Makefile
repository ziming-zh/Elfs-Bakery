elm.js: $(wildcard src/*.elm)
	elm make src/Main.elm --output=elm.js
	
.PHONY=clean
clean:
	rm public/elm.js
