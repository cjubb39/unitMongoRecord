#Makefile

#choose compiler
CC = gcc
CXX = g++

#compilation options
CFLAGS = -g -Wall
CXXFLAGS = -g - Wall

#linking options
LDFLAGS = -g

#incorporates math.h
LDLIBS = -lm

#define main and dependencies
complete: enterUnits endLineScrub

enterUnits: enterUnits.o fileOperations.o

enterUnits.o: enterUnits.c fileOperations.c fileOperations.h

fileOperations.o: fileOperations.c fileOperations.h

endLineScrub: endLineScrub.c

#clean targer
.PHONY: clean
clean:
	rm -f *.o enterUnits endLineScrub

.PHONY: all
all: clean complete clean