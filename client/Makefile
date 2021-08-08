.PHONY: live
live:
	elm-live src/Main.elm --open -- --output elm.js

.PHONY: live-debug
live-debug:
	elm-live src/Main.elm --open -- --output elm.js --debug

.PHONY: github-pages
github-pages:
	elm make src/Main.elm --optimize --output elm.js
	cp elm.js index.html /tmp/
	git checkout github-pages
	cp /tmp/elm.js /tmp/index.html ..
	git add ../elm.js ../index.html
	git commit -m'Deploy to github pages'

