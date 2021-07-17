STEPS =

all: res

node_modules:
	npm install

src/step%.bs.js: node_modules src/step%.res
	./node_modules/.bin/rescript build

.PHONY: res clean

res: $(foreach s,$(STEPS),src/$(s).bs.js)

clean:
	rm -f src/*.js
