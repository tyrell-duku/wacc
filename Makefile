# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler
# Locations

# Tools

# the make rules

all: rules

rules:
	sbt assembly

clean:
	sbt clean
	rm -rf *.s

test:
	sbt test

.PHONY: all rules clean test


