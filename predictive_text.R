library(readtext)
library(quanteda)
qopts <- quanteda_options()
quanteda_options(threads = 3)

## Create train and test subsets of data ----

paths <-
    unzip("Coursera-SwiftKey.zip") # con/path to files in archive
dlist <- lapply(paths[7:9], readLines, skipNul = T)
train_size <-
    50000 # how many lines to extract from each document for training
test_size <- 1000
train_index <- lapply(dlist, function(x) {
    sample(1:length(x), train_size, replace = F)
})

the_names <- c("blogs", "news", "twitter")
set.seed(42)
train_sample <- list()
test_sample <- list()

for (i in 1:length(train_index)) {
    train_sample[[the_names[i]]] <- dlist[[i]][train_index[[i]]]
    test_sample[[the_names[i]]] <- dlist[[i]][-train_index[[i]]]
    test_sample[[the_names[i]]] <-
        test_sample[[the_names[i]]][sample(1:length(test_sample[[the_names[i]]]), 
                                           test_size, 
                                           replace = F)]
}

for (i in the_names) {
    write(train_sample[[i]], file = paste0("./corpus_train_EN/", i, ".txt"))
    write(test_sample[[i]], file = paste0("./corpus_test_EN/", i, ".txt"))
}

rm(list = ls())

texts_train <- readtext("./corpus_train_EN/*.txt") # use reduced set
corpus_train <- corpus(texts(texts_train))
corpus_train <- corpus(texts(corpus_train,
                             groups = rep(1, ndoc(corpus_train)))) # collapse documents

rm(texts_train)

tokens_train <- corpus_train %>%
    tokens(
        remove_symbols = T,
        remove_punct = T,
        remove_separators = T,
        remove_numbers = T,
        remove_twitter = T,
        remove_url = T
    ) %>%
    tokens_tolower

rm(corpus_train)

texts_test <- readtext("./corpus_test_EN/*.txt")
corpus_test <- corpus(texts(texts_test))
corpus_test <- corpus(texts(corpus_test,
                            groups = rep(1, ndoc(corpus_test)))) # collapse documents

rm(texts_test)

tokens_test <- corpus_test %>%
    tokens(
        remove_symbols = T,
        remove_punct = T,
        remove_separators = T,
        remove_numbers = T,
        remove_twitter = T,
        remove_url = T,
        ngrams = c(2:5),
        concatenator = " "
    ) %>%
    tokens_tolower

rm(corpus_test)

# Strip infrequent tokens out of all tokens
# This reduces sparcity and massively reduces matrix memory requirements

dfm_freq_plot <- function(dfm) {
    library(ggplot2)
    features_dfm <- textstat_frequency(dfm, 100)
    features_dfm$stopword <- features_dfm$feature %in% stopwords()
    features_dfm$feature <-
        with(features_dfm, reorder(feature,-frequency))
    ggplot(features_dfm, aes(x = feature, y = frequency, color = stopword)) +
        geom_point() +
        theme(
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid.major = element_blank(),
            legend.position = "none"
        )
}

plot_coverage <- function(dfm) {
    library(ggplot2)
    features_dfm <- textstat_frequency(dfm)
    features_dfm$feature <-
        with(features_dfm, reorder(feature, frequency))
    
    per_word_instance <- NA_real_
    unique_words <- NA_integer_
    total_feature_frequency <- sum(features_dfm$frequency)
    for (i in 1:100) {
        unique_words[i] <- round(length(features_dfm$feature) * i / 100, 0)
        the_words <-
            as.character(features_dfm$feature[1:unique_words[i]])
        freq_of_the_words <-
            sum(features_dfm[1:unique_words[i], "frequency"])
        per_word_instance[i] <-
            freq_of_the_words / total_feature_frequency * 100
    }
    
    ggplot(
        data.frame(unique_words, per_word_instance),
        aes(x = unique_words, y = per_word_instance)
    ) +
        geom_line() +
        ggtitle("Coverage")
}

dfm_1gram <- dfm(tokens_train)

# Plot coverage to determine how many words to keep 
# (ie remove words that provide little value)

dfm_freq_plot(dfm_1gram)
plot_coverage(dfm_1gram)

# The inflection point seems to be at about 10000 words, over 98% of word
# instances are covered Trimming beyond these words significantly reduced tokens
# and memory load.

dfm_1gram_trim <- dfm_trim(dfm_1gram,
                           min_termfreq = 5000,
                           termfreq_type = "rank")

frequent_words <- names(topfeatures(dfm_1gram_trim, n = 20))

tokens_train <- tokens_select(tokens_train, 
                              dfm_1gram_trim@Dimnames$features, 
                              padding = F)

# Create token ngrams lists:

ngram_list <- list(ngram_1 = tokens_train)
rm(tokens_train, dfm_1gram, dfm_1gram_trim)

for (ngram in 2:5) {
    the_tokens <-
        tokens(ngram_list[["ngram_1"]], ngrams = ngram, concatenator = " ")
    the_dfm <- dfm(the_tokens)
    the_dfm <-
        dfm_trim(the_dfm,
                 min_termfreq = 2,
                 termfreq_type = "count")
    ngram_list[[paste0("ngram_", ngram)]] <-
        tokens_select(the_tokens, the_dfm@Dimnames$features, padding = F)
}

rm(the_dfm, the_tokens)

# # Make frequency tables and apply Good-Turing discounting to smooth data

gt_discount <- function(tokens, remove_singles = T) {
    the_counts <- table(tokens[[1]])
    # remove tokens that only appear once
    if (remove_singles == T)
        the_counts <- the_counts[the_counts > 1]
    new_counts <- c(NA_real_)
    for (i in 2:5) {
        new_counts[i] <- (i + 1) * ifelse(sum(the_counts == i) != 0,
                                          sum(the_counts == i + 1) / sum(the_counts == i),
                                          1)
    }
    for (i in 2:5) {
        the_counts[the_counts == i] <- new_counts[i]
    }
    the_counts
}

smoothed_ngrams <- list()
for (i in 1:5) {
    smoothed_ngrams[[paste0("ngram_", i)]] <-
        gt_discount(ngram_list[[paste0("ngram_", i)]])
}

rm(ngram_list)

# Up to here just to get ngram frequency data, smoothed_ngrams, for model

## Functions for next_word predition ----

make_tokens <- function(the_string, remove_stopwords = F) {
    library(stringi)
    library(quanteda)
    
    #use same token process as for corpus
    the_tokens <- the_string %>%
        tokens(
            remove_symbols = T,
            remove_punct = T,
            remove_separators = T,
            remove_numbers = T,
            remove_twitter = T,
            remove_url = T
        ) %>%
        tokens_tolower %>%
        {
            if (remove_stopwords == T)
                tokens_remove(., stopwords())
            else
                .
        } %>%
        .[[1]] %>%
        rev(.) %>%
        .[1:4] %>%
        .[!is.na(.)]
    the_tokens
}


next_word_backoff_search <- function(the_string,
                                     max_gram = 3,
                                     remove_stopwords = F) {
    if (!grepl("[[:alnum:]]", the_string)) {
        return(sample(frequent_words, 4, replace = F))
    }
    # max_gram is largest ngram that will be used for next work lookup
    # in the max_gram + 1 smoothed_ngrams table
    
    the_tokens <- make_tokens(the_string,
                              remove_stopwords = remove_stopwords)
    
    #  !! NOTE !!  This REVERSES WORD ORDER for easier subsetting to last words
    
    # start with largest ngram and back-off until match is found
    # i = num *complete* words = current ngram length. Lookup will be i + 1 ngram.
    i <- length(the_tokens)
    # j, add a word to lookup phrase but don't increase ngram
    # if last word is incomplete
    j <-
        ifelse(grepl("[[:space:][:punct:]]$", the_string), 0, 1)
    i <- ifelse(i > max_gram, max_gram, i) # restrict lookup
    ngrams <- as.numeric(NULL)
    while (length(ngrams) == 0 & i > 0) {
        the_phrase <- paste0("^", paste0(the_tokens[i:1], collapse = " "))
        if (i - j == 0)
            the_phrase <-
                gsub("[[:space:][:punct:]]$", "", the_phrase)
        ngrams <-
            smoothed_ngrams[[i - j + 1]][stri_detect_regex(names(smoothed_ngrams[[i - j + 1]]),
                                                           the_phrase)] # faster than grep()
        i <- i - 1
    }
    
    if (length(ngrams > 0)) {
        next_words <- stri_extract_last_words(names(ngrams)[order(ngrams,
                                                                  decreasing = T)][1:4])
    } else {
        next_words <- sample(frequent_words, 4, replace = F)
    }
    
    if (any(is.na(next_words))) {
        next_words[is.na(next_words)] <- sample(frequent_words,
                                                length(next_words[is.na(next_words)]))
    }
    next_words
}

## Test effectiveness of next word model

library(data.table)
library(lineprof)
validate_model <- function() {
    validation_dt <-
        data.table(
            ngram = sample(tokens_test[[1]], 1000, replace = F),
            words = NA_integer_,
            score = NA_integer_
        )
    for (i in 1:nrow(validation_dt)) {
        test_ngram <- validation_dt[i, ngram]
        num_words_test <- stri_count_boundaries(test_ngram)
        next_word_test <- stri_extract_last_words(test_ngram)
        the_string_test <- gsub(next_word_test, "", test_ngram)
        next_words <- next_word_backoff_search(the_string_test)
        the_res <- which(grepl(next_word_test, next_words))
        if (sum(the_res) == 0) {
            the_score <- 0
        } else if (1 %in% the_res) {
            the_score <- 4
        } else if (2 %in% the_res) {
            the_score <- 3
        } else if (3 %in% the_res) {
            the_score <- 2
        } else {
            the_score <- 1
        }
        validation_dt[ngram == test_ngram, `:=`(words = num_words_test,
                                                score = the_score)]
    }
    validation_dt
}
