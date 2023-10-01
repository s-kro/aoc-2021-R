#!/usr/bin/Rscript


# Advent of Code Day 7 Part 1

crabs <- as.numeric(read.csv("aoc_2021-7.dat", header = FALSE))
sum(replace(crabs, TRUE, abs(crabs - median(crabs))))
