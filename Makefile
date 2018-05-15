.PHONY: build

build:
	docker build -t lester/dev-environment . --no-cache

run:
	docker run -i -t lester/dev-environment /bin/zsh


