# default build target
all::

.PHONY: all
all:: build

.PHONY: build
build:

	docker build --tag heroku-docker-r-example:shiny .

.PHONY: run
run:

	docker run -it -p "8080:8080" heroku-docker-r-example:shiny

.PHONY: test
test:

	@curl -v "localhost:8080"
