elm.js: $(wildcard src/*.elm)
	elm make src/Main.elm --output=./build/main.js --debug
	
.PHONY=clean
clean:
	rm public/main.js
