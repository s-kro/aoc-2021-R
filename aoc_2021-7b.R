#!/usr/bin/Rscript



# Advent of Code Day 7 Part 2

f <- function(d) {(d**2 + d) / 2}

crabs <- as.numeric(read.csv("aoc_2021-7.dat", header = FALSE))
sum(f(abs(crabs - floor(mean(crabs)))))
