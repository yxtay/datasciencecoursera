setwd("~/GitHub/datasciencecoursera/10-data-science-capstone/")

###
# load packages ----
###

library(plyr); library(dplyr); library(data.table)
library(stringi); library(tm); library(stringdist); library(wordcloud)
library(ggplot2); library(gridExtra); library(RColorBrewer)
library(knitr); library(shiny); library(shinyapps)
theme_set(theme_bw())

###
# milestone report ----
###

# knit2html("dsc-milestone.Rmd")

###
# shiny app ----
###

# deployApp("shiny", "dsc-shiny")

###
# read text ----
###

fnames <- c("blogs", "news", "twitter")
files <- paste0("./final/en_US/en_US.", fnames, ".txt")
system.time(texts <- llply(files, readLines, skipNul = T))
names(texts) <- fnames

###
# explore ----
###

size <- sapply(texts, object.size)
line.cnt <- laply(texts, length)
char.cnt <- llply(texts, stri_length)
char.total <- laply(char.cnt, sum)
df.exp <- data.frame(line.cnt, size, char.total, 
                     size.mean = size / line.cnt, 
                     char.mean = char.total / line.cnt)

###
# preprocess ----
###

system.time(sentences <- llply(texts, stri_split_boundaries, type = "sentence"))
sentences <- llply(sentences, unlist)
rm(texts); gc()

clean_text <- function(s) {
    # convert encoding to ascii
    s <- iconv(s, to = "ascii", sub = "")
    # convert to lower case
    s <- stri_trans_tolower(s)
    # remove punctuations
    s <- removePunctuation(s)
    return(s)
}

system.time(sentences <- llply(sentences, clean_text))
system.time(words <- llply(sentences, stri_split_boundaries, type = "word", skip_word_none = T))
rm(sentences); gc()
save(words, file = "RData/processed_words.RData")

###
# visualisation
###

load("RData/processed_words.RData")
# get word length and prepare data frame
system.time(word.cnt <- llply(words, laply, length))
df.wc <- ldply(word.cnt, data.frame)
setnames(df.wc, c("corpus", "word.count"))
# prepare data frame for character length
df.cc <- ldply(char.cnt, data.frame)
setnames(df.cc, c("corpus", "char.count"))

# char viz

p1 <- qplot(char.count, data = df.cc, geom = "histogram", fill = corpus,
            xlim = c(0, 1000), binwidth = 50,
            xlab = "Character Count", 
            ylab = "Frequency")

p2 <- qplot(char.count, data = subset(df.cc, corpus == "blogs"), 
            xlim = c(0, 1200), binwidth = 50,
            main = "Blogs",
            xlab = "Character Count", 
            ylab = "Frequency")

p3 <- qplot(char.count, data = subset(df.cc, corpus == "news"),
            xlim = c(0, 1000), binwidth = 50,
            main = "News",
            xlab = "Character Count", 
            ylab = "Frequency")

p4 <- qplot(char.count, data = subset(df.cc, corpus == "twitter"),
            xlim = c(0, 150), binwidth = 10,
            main = "Twitter",
            xlab = "Character Count", 
            ylab = "Frequency")

grid.arrange(p1, p2, p3, p4, ncol = 2)
rm(p1, p2, p3, p4); gc()

# word viz

p1 <- qplot(word.count, data = df.wc, geom = "histogram", fill = corpus,
            xlim = c(0, 150), binwidth = 5,
            xlab = "Word Count", 
            ylab = "Frequency")

p2 <- qplot(word.count, data = subset(df.wc, corpus == "blogs"), 
            xlim = c(0, 210), binwidth = 5,
            main = "Blogs",
            xlab = "Word Count", 
            ylab = "Frequency")

p3 <- qplot(word.count, data = subset(df.wc, corpus == "news"), 
            xlim = c(0, 120), binwidth = 5,
            main = "News",
            xlab = "Word Count", 
            ylab = "Frequency")

p4 <- qplot(word.count, data = subset(df.wc, corpus == "twitter"),
            xlim = c(0, 40), binwidth = 2,
            main = "Twitter",
            xlab = "Word Count", ylab = "Frequency")

grid.arrange(p1, p2, p3, p4, ncol = 2)
rm(p1, p2, p3, p4); gc()

###
# word count ----
###

load("RData/processed_words.RData")
process_words <- function(wv) {
    # use empty string to denote start of sentence
    wv <- llply(wv, function(v) c("", v))
    # covert to a single vector
    wv <- unlist(wv)
    # do preprocessing on numbers
    wv <- stri_replace_all_regex(wv, "^[[:digit:]]+$", "<NUM>")
    wv <- stri_replace_all_regex(wv, "[[:digit:]]+", "")
    # convert into factor
    wv <- factor(wv)
    # remove consecutive "<NUM>" tokens
    nidx <- wv != "<NUM>"
    wv <- wv[nidx | lead(nidx, 1)]
    # remove consecutive empty string tokens
    nidx <- wv != ""
    wv <- wv[nidx | lead(nidx, 1)]
}
system.time(words <- llply(words, process_words))
save(words, file = "RData/factored_words.RData")

load("RData/factored_words.RData")
# compute word frequencies and convert to data table
word_freq <- function(w) {
    word_fq <- data.table(word = w)[, .N, by = word][order(-N)]
}
word.tbl <- data.table(ldply(words, word_freq))
setnames(word.tbl, ".id", "corpus")

# compute proportion metrics
setkey(word.tbl, corpus)
word.tbl <- word.tbl[
    , copy(.SD)[, `:=` (N.prop = N / sum(N), 
                        N.prop.cum = cumsum(N) / sum(N), 
                        rank = .I)], 
    by = corpus]
word.tbl[, `:=` (word = reorder(word, -N.prop, sum))]
save(word.tbl, file = "RData/word_table.RData")

###
# word cloud ----
###

load("RData/word_table.RData")
op <- par(mfrow = c(1, 3))
l_ply(names(words), function(c) {
    with(word.tbl[corpus == c & word != ""], 
         wordcloud(word, N, scale = c(5, 0.5), max.words = 200,
                   colors = brewer.pal(9, "Set1")))
    text(0.5, 1, labels = c, cex = 2)
})
par(op)

###
# zipf's law ----
###

qplot(rank, N, data = word.tbl[word != ""],
      color = corpus, group = corpus,
      log = "xy",
      main = "Zipf's Law",
      xlab = "Word Rank",
      ylab = "Word Frequency")

###
# cleaning for ngrams ----
###

# identify and remove words < 5

load("RData/word_table.RData")
load("RData/factored_words.RData")
words <- llply(words, factor, levels = levels(word.tbl$word))
word.tbl[N > 5, max(N.prop.cum), by = corpus]

common.words <- with(word.tbl, levels(droplevels(word[N > 5])))
rare.words <- setdiff(levels(word.tbl$word), common.words)

words <- llply(words, mapvalues, from = rare.words, to = rep("<RARE>", length(rare.words)), warn_missing = F)
word.index <- c(common.words, "<RARE>")
words <- llply(words, factor, levels = word.index)
save(words, file = "RData/reordered_words.RData")

###
# ngrams count ----
###

load("RData/reordered_words.RData")
load("RData/word_index.RData")
words <- llply(words, as.integer)
ngram_table <- function(tokens, n = 5) {
    tokens.list <- llply((n-1):0, lag, x = tokens)
    ngrams <- do.call(data.table, tokens.list)
    setnames(ngrams, 1:n, c(paste0("wordm", (n-1):1), "pred"))
    setkeyv(ngrams, names(ngrams))
    ngram.table <- ngrams[, .N, by = key(ngrams)][order(-N)]
    ngram.table[!(pred %in% match(c("", "<RARE>", "<NUM>"), word.index)), `:=` (n = n)]
}
system.time(ngram.table <- llply(words, ngram_table))
rm(words); gc()
save(ngram.table, file = "RData/ngram_table.RData")

###
# ngrams proportion ----
###

load("RData/ngram_table.RData")
load("RData/word_index.RData")
reduced_ngram_table <- function(ngram.table, rn, n = 5) {
    rn <- min(rn, n)
    coln <- names(ngram.table)
    # ngram.table <- copy(ngram.table)
    if (rn >= n) return(ngram.table)
    setkeyv(ngram.table, c(paste0("wordm", (rn-1):1), "pred"))
    rngram.table <- ngram.table[, .(N = sum(N)), by = key(ngram.table)]
    rngram.table[, `:=` (n = rn)]
    na.col <- setdiff(coln, names(rngram.table))
    rngram.table[, eval(na.col) := NA]
    setcolorder(rngram.table, coln)[order(-N)]
}

calc_prob <- function(ngram.table, n = 5) {
    ngram.table <- copy(ngram.table)
    ngram.table[, pred := ifelse(N<=1, match("<RARE>", word.index), pred)]
    setkeyv(ngram.table, c(paste0("wordm", (n-1):1), "n"))
    ngram.table[, `:=` (t = sum(N)), by = key(ngram.table)]
    ngram.table[, `:=` (p = N / t)]
    ngram.table[N > 5 & t > 5 & pred != match("<RARE>", word.index)]
}

prob_table <- function(ngram.table, rn) {
    tbl <- ngram.table
    tbl <- reduced_ngram_table(tbl, rn)
    gc()
    tbl <- calc_prob(tbl)
}

prob.table <- llply(ngram.table, prob_table, rn = 2)

prob.table <- data.table(ldply(prob.table, I))
setnames(prob.table, ".id", "corpus")
emp <- match("", word.index)
rare <- match("<RARE>", word.index)

# pentagram, rn = 5
# pentagram <- prob.table[wordm4 != rare & wordm3 != emp & wordm2 != emp & wordm1 != emp]
# save(pentagram, file = "RData/pentagram.RData")
# tetragram, nr = 4
# tetragram <- prob.table[wordm3 != rare & wordm2 != emp & wordm1 != emp]
# save(tetragram, file = "RData/tetragram.RData")
# trigram, rn = 3
# trigram <- prob.table[wordm2 != rare & wordm1 != emp]
# save(trigram, file = "RData/trigram.RData")
# bigram, rn = 2
# bigram <- prob.table[wordm1 != rare]
# save(bigram, file = "RData/bigram.RData")
# rm(list = ls()); gc()

unigram_table <- function(ngram.table, n = 5) {
    coln <- names(ngram.table)
    setkey(ngram.table, pred)
    emp <- match("", word.index)
    unigram.table <- ngram.table[pred != emp, .(N = length(unique(wordm1))), by = key(ngram.table)]
    unigram.table[, `:=` (n = 1)]
    na.col <- paste0("wordm", (n-1):1)
    unigram.table[, eval(na.col) := NA]
    setcolorder(unigram.table, coln)
}

prob.table <- llply(ngram.table, function(tbl) {
    tbl <- unigram_table(tbl)
    gc()
    tbl <- calc_prob(tbl)
})

# unigram, rn = 1
# unigram <- prob.table
# save(unigram, file = "RData/unigram.RData")

fnames <- c("pentagram", "tetragram", "trigram", "bigram", "unigram")
l_ply(paste0("RData/", fnames, ".RData"), load, envir = environment())
prob.table <- rbindlist(llply(fnames, get))[order(-p)]
setkeyv(prob.table, c("corpus", paste0("wordm", 4:1)))
prob.table <- prob.table[, head(.SD, 100), by = key(prob.table)]
setkeyv(prob.table, c(paste0("wordm", 4:1), "pred"))
save(prob.table, file = "RData/prob_table.RData")

# move files to folder for shiny app

file.copy("RData/prob_table.RData", "shiny/RData/prob_table.RData")
file.copy("RData/word_index.RData", "shiny/RData/word_index.RData")

###
# prediction ----
###

load("RData/prob_table.RData")
load("RData/word_index.RData")

process_input <- function(s) {
    # split into sentences
    s <- stri_split_boundaries(s, type = "sentence")
    s <- unlist(lapply(s, function(s) {
        # convert encoding to ascii
        s <- iconv(s, to = "ascii", sub = "")
        # convert to lower case
        s <- stri_trans_tolower(s)
        # remove punctuations
        s <- stri_replace_all_regex(s, "[[:punct:]]", "")
        # split into words
        s <- stri_split_boundaries(s, type = "word", skip_word_none = T)
        # pad with empty string at beginning of sentence
        s <- c("", s)
    }))
    # handle numbers
    s <- stri_replace_all_regex(s, "^[[:digit:]]+$", "<NUM>")
    s <- stri_replace_all_regex(s, "[[:digit:]]+", "")
}

input_table <- function(s, n = 4) {
    # obtain last n words from processed input
    s <- tail(s, n)
    # replace words with numbers using word.index
    s <- match(s, word.index, nomatch = match("<RARE>", word.index))
    # pad with NA is word count < n
    if (length(s) < n) {s <- c(rep(NA, n - length(s)), s)}
    # create data.table from processed input and name columns appropriately
    s.tbl <- as.data.table(as.list(s))
    setnames(s.tbl, 1:n, paste0("wordm", n:1))
    # create a few copies of the previous tables with first few words removed.
    s.tbl <- rbindlist(lapply(0:n, function(i) {
        if (i == 0) {return(copy(s.tbl))}
        copy(s.tbl)[, eval(names(s.tbl)[1:i]) := NA]
    }))
    unique(setkeyv(s.tbl, names(s.tbl)))
}

rank_pred <- function(s.tbl, prob.table, n.pred = 5, cp.weights = c(1, 1, 1), ng.weights = (1:5) ^ 2) {
    # corpus weights to be referred by name
    names(cp.weights) <- c("blogs", "news", "twitter")
    # combine prediction probabilities from different corpus by cp.weights
    tbl <- prob.table[s.tbl, .(p = sum(p * cp.weights[corpus]) / sum(cp.weights)), 
                      by = c(key(prob.table), "n"), nomatch = 0]
    # obtain size of largest n-gram in predictions
    mn <- max(tbl[["n"]])
    if (is.null(mn)) mn <- 1
    # combine prediction probabilities from different n-gram size by ng.weights
    tbl <- tbl[, .(p = sum(p * ng.weights[n]) / sum(ng.weights[1:mn])), by = pred]
    # replace predictions with actual word and take the largest n.pred ones ordered by probabilities
    tbl[, `:=` (pred = word.index[pred])][order(-p)[1:min(.N, n.pred)]]
}

###
# quizzes ----
###

options_dt <- function(s, op, n = 4) {
    op <- unlist(llply(op, function(word) {
        word.index[stringdist(word, word.index) <= 1]
    }))
    s.dt <- predict_dt(s, n)
    op <- match(op, word.index, nomatch = match("<RARE>", word.index))
    s.dt <- s.dt[, .(pred = op), by = names(s.dt)]
    setkeyv(s.dt, names(s.dt))
}

rank_options <- function(s, op, prob.table) {
    s <- process_input(s)
    s.dt <- options_dt(s, op)
    rank_pred(s.dt, prob.table, 10)
}

# quiz 2 ----

# 1
s <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
op <- c("beer", "pretzels", "cheese", "soda")
rank_options(s, op, prob.table)

# 2
s <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
op <- c("universe", "most", "best", "world")
rank_options(s, op, prob.table)

# 3
s <- "Hey sunshine, can you follow me and make me the"
op <- c("bluest", "saddest", "happiest", "smelliest")
rank_options(s, op, prob.table)

# 4
s <- "Very early observations on the Bills game: Offense still struggling but the"
op <- c("players", "crowd", "defense", "referees")
rank_options(s, op, prob.table)

# 5
s <- "Go on a romantic date at the"
op <- c("beach", "movies", "mall", "grocery")
rank_options(s, op, prob.table)

# 6
s <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
op <- c("phone", "horse", "way", "motorcycle")
rank_options(s, op, prob.table)

# 7
s <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
op <- c("thing", "time", "year", "week")
rank_options(s, op, prob.table)

# 8
s <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
op <- c("ears", "toes", "fingers", "eyes")
rank_options(s, op, prob.table)

# 9
s <- "Be grateful for the good times and keep the faith during the"
op <- c("sad", "hard", "worst", "bad")
rank_options(s, op, prob.table)

# 10
s <- "If this isn't the cutest thing you've ever seen, then you must be"
op <- c("insane", "callous", "asleep", "insensitive")
rank_options(s, op, prob.table)

# quiz 3 ----

# 1
s <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
op <- c("die", "eat", "sleep", "give")
rank_options(s, op, prob.table)

# 2
s <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
op <- c("spiritual", "financial", "horticultural", "marital")
rank_options(s, op, prob.table)

# 3
s <- "I'd give anything to see arctic monkeys this"
op <- c("weekend", "decade", "month", "morning")
rank_options(s, op, prob.table)

# 4
s <- "Talking to your mom has the same effect as a hug and helps reduce your"
op <- c("stress", "happiness", "sleepiness", "hunger")
rank_options(s, op, prob.table)

# 5
s <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
op <- c("picture", "look", "minute", "walk")
rank_options(s, op, prob.table)

# 6
s <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
op <- c("incident", "case", "matter", "account")
rank_options(s, op, prob.table)

# 7
s <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
op <- c("arm", "toe", "finger", "hand")
rank_options(s, op, prob.table)

# 8
s <- "Every inch of you is perfect from the bottom to the"
op <- c("middle", "top", "center", "side")
rank_options(s, op, prob.table)

# 9
s <- "I'm thankful my childhood was filled with imagination and bruises from playing"
op <- c("inside", "daily", "weekly", "outside")
rank_options(s, op, prob.table)

# 10
s <- "I like how the same people are in almost all of Adam Sandler's"
op <- c("movies", "novels", "stories", "pictures")
rank_options(s, op, prob.table)