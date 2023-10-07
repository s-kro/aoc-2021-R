#!/usr/bin/Rscript



# Advent of Code Day 8 Part 1

f <- function(n) {ifelse (n == 2 | n == 3 | n == 4 | n == 7, 1, 0)}

segs <- read.table("aoc_2021-8.dat", sep = '|',  header = FALSE)
disp <- data.frame(do.call('rbind', strsplit(trimws(segs$V2), ' ', fixed = FALSE)))
sum(sapply(disp, function(x) f(nchar(x))))
