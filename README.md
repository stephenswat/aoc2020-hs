# Advent of Code 2020

This repository contains my Haskell solutions to the Advent of Code 2020
puzzles. You can find out more about Advent of Code at
https://adventofcode.com/2020/.

## Build

Building this code should be trivial if you have Stack installed. The following
command should be sufficient to build the project:

```bash
$ stack build
```

## Run

Similarly, you can use `stack run` to run the project. The program takes two
arguments, which are the day number you wish to execute, and an input file. For
example, to run the solution for day 1, execute the following command:

```bash
$ stack run 1 input/Day01.txt
```

## Reflections

The problems this year seemed significantly easier than the problems of last
year. I am not sure whether that is just due to me having a better understanding
of Haskell this year. The only problem that gave me some trouble was day 20,
which I didn't find the motivation to solve for a while.

It surprised me that there was no set of problems with a common theme like there
was last year with the Intcode puzzles. There were more automaton puzzles than
last year, but they didn't seem to form a cohesive thread of problems increasing
in difficulty.

The only problem I did not solve in Haskell was part B of day 23. I solved that
in C++, because I needed the performance of a linked list and I wasn't sure how
to model the data structures necessary in a performant manner.