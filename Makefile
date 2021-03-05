# Use Bash instead of sh
SHELL := /bin/bash

lint:
	sbt scalastyle

test:
	sbt test