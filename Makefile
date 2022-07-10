elm.js: $(wildcard src/*.elm)
	elm make src/Main.elm --output=main.js
	
.PHONY=clean
clean:
	rm public/main.js
