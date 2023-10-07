#!/usr/bin/Rscript


## Advent of Code Day 8 Part 2

segs <- read.table("aoc_2021-8.dat", sep = '|',  header = FALSE)
segs$V1 <- data.frame(do.call('rbind', strsplit(trimws(segs$V1), ' ', fixed = FALSE)))
segs$V2 <- data.frame(do.call('rbind', strsplit(trimws(segs$V2), ' ', fixed = FALSE)))

gt <- 0 # grand total
for(i in 1:nrow(segs)) { ## we can deduce any character in the output by just its number
    ## of segments illuminated in the case of 1, 4, 7, and 8, or a combination of number
    ## of segments combined with number of segments of c and f and number of segments of
    ## b and d, even though we never figure out exactly which segment is a, b, c, or f.

    one  <- as.character(segs[i, ]$V1[which(nchar(segs[i, ]$V1) == 2)])
    four <- as.character(segs[i, ]$V1[which(nchar(segs[i, ]$V1) == 4)])

    bd <- four # remove segments c and f from the 4 character to get b and d
    for (j in unlist(strsplit(four, ''))) {
        for (k in unlist(strsplit(one, ''))) {
            if (j == k) { bd <- sub(k, '', bd) }
        }
    }

    t <- 0 # row total
    for (j in 1:ncol(segs$V2)) { # just a brute force loop though the columns
        n <- nchar(segs$V2[[i, j]]) # number of segments illuminated
        c <- 0 # digit "character" value
        if      (n == 2) { c <- 1 } # one
        else if (n == 3) { c <- 7 } # seven
        else if (n == 4) { c <- 4 } # four
        else if (n == 7) { c <- 8 } # eight
        else {  #  n == 5 | n == 6

            m_t  <- 0 # match three
            m_f  <- 0 # match five
            m_bd <- 0 # match bd
            m_cf <- 0 # match cf

            for (l in unlist(strsplit(bd, ''))) { # contains segments of "1" (b and d)?
                m <- lengths(regmatches(segs$V2[[i, j]], gregexpr(l, segs$V2[[i, j]])))
                if (n == 5) { m_f <- m_f + m }
                else       { m_bd <- m_bd + m } # 6 segmengts illuminated 
            }
            for (l in unlist(strsplit(one, ''))) {
                m <- lengths(regmatches(segs$V2[[i, j]], gregexpr(l, segs$V2[[i, j]])))
                if (n == 5) { m_t <- m_t + m }
                else { m_cf <- m_cf + m }
            }
            if (m_t  == 2 & m_f  == 1) { c <- 3} # three
            if (m_t  == 1 & m_f  == 2) { c <- 5} # five
            if (m_t  == 1 & m_f  == 1) { c <- 2} # two
            if (m_bd == 1 & m_cf == 2) { c <- 0} # zero
            if (m_bd == 2 & m_cf == 1) { c <- 6} # six
            if (m_bd == 2 & m_cf == 2) { c <- 9} # nine

        }
        t <- t + c * 10^(4 - j)
    }
    gt <- gt + t
}
gt

