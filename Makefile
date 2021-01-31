# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler
# Locations

# Tools

# the make rules

all: rules

# runs the antlr build script then attempts to compile all .java files within src
rules:
	sbt compile

clean:
	sbt clean

test:
	sbt test

.PHONY: all rules clean test


