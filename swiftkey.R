
library(readtext)
library(quanteda)

## Create sample of data to practice with
# paths <- unzip("Coursera-SwiftKey.zip") # con/path to files in archive
# dlist <- lapply(paths[7:9], readLines, skipNul = T)
# sample_size <- 50000
# set.seed(42)
# dsample <- lapply(dlist, function(x){
#     x[sample(1:length(x), sample_size, replace = F)]
# })
# the_names <- c("blogs", "news", "twitter")
# names(dsample) <- the_names
# for(i in the_names){
#     write(dsample[[i]], file = paste0("./sampled_EN/", i, ".txt"))
# }

qopts <- quanteda_options()
quanteda_options(threads = 3)
the_texts <- readtext("./final/en_US/*.txt") #readtext("./sampled_EN/*.txt") # 

c_corpus <- corpus(texts(the_texts)) 
c_corpus <- corpus(texts(c_corpus, groups = rep(1, ndoc(c_corpus)))) # collapse documents

my_tokens <- c_corpus %>% 
    tokens(remove_symbols = T,
           remove_punct = T,
           remove_separators = T, 
           remove_numbers = T,
           remove_twitter = T,
           remove_url = T) %>%
    tokens_tolower
rm(the_texts)
rm(c_corpus)

# Strip infrequent tokens out of my_tokens
# reduces sparcity and massively reduces matrix memory requirements

dfm_freq_plot <- function(dfm){
    library(ggplot2)
    features_dfm <- textstat_frequency(dfm, 100)
    features_dfm$stopword <- features_dfm$feature %in% stopwords()
    features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))
    ggplot(features_dfm, aes(x = feature, y = frequency, color = stopword)) +
        geom_point() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1), 
              panel.grid.major = element_blank(),
              legend.position="none")
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

dfm_1gram <- dfm(my_tokens)
# Plot coverege to determine how many words to keep (ie remove words that provide little value)
dfm_freq_plot(dfm_1gram)
plot_coverage(dfm_1gram)

# The inflection point seems to be at about 10000 words, over 98% of word instances are covered
# Trimming beyond these words significantly reduced tokens and memory load. 

dfm_1gram <- dfm_trim(dfm_1gram, 
                    min_termfreq = 5000, 
                    termfreq_type = "rank")

my_tokens <- tokens_select(my_tokens, dfm_1gram@Dimnames$features, padding = F) 
rm(dfm_1gram)
# Create token ngrams lists:
ngram_list <- list(ngram_1 = my_tokens)
rm(my_tokens)

for(ngram in 2:4){
    the_tokens <- tokens(ngram_list[["ngram_1"]], ngrams = 2, concatenator = " ")
    the_dfm <- dfm(the_tokens)
    the_dfm <- dfm_trim(the_dfm, min_termfreq = 2, termfreq_type = "count")
    ngram_list[[paste0("ngram_",ngram)]] <- tokens_select(the_tokens, the_dfm@Dimnames$features, padding = F)
}

rm(the_dfm, the_tokens)

# Make frequency tables and apply Good-Turing discounting to smooth data
gt_discount <- function(tokens){
    the_counts <- table(tokens[[1]])
    the_counts <- the_counts[the_counts > 1] # remove tokens that only appear once
    new_counts <- c(NA_real_)
    for(i in 2:5){
        new_counts[i] <- (i + 1) * ifelse(sum(the_counts == i) != 0, 
                                          sum(the_counts == i + 1) / sum(the_counts == i),
                                          1)
    }
    for(i in 2:5){
        the_counts[the_counts == i] <- new_counts[i]
    }
    the_counts
}

smoothed_ngrams <- list()
for(i in 1:4){
    smoothed_ngrams[[paste0("ngram_",i)]] <- gt_discount(ngram_list[[paste0("ngram_",i)]])
}
rm(ngram_list)

# Up to here just to get ngram frequency data, smoothed_ngrams, for model ----

next_word_backoff_search <- function(the_string){
    
    library(stringi)
    library(quanteda)
    
    #use same token process as for corpus
    the_tokens <- the_string %>% 
        tokens(remove_symbols = T,
               remove_punct = T,
               remove_separators = T, 
               remove_numbers = T,
               remove_twitter = T,
               remove_url = T) %>%
        tokens_tolower %>% .[[1]] %>% .[1:4] %>% rev(.) %>% .[!is.na(.)]
    # Is input complete?
    grepl("[[:space:][:punct:]]$", the_string)
    
    ## !! NOTE !! readline() strips trailing whitespace so this doesn't quite
    ## work the way I want yet. For now always terminate with a punctuation
    ## char. Possible shiny app won't do this OR Ill have to rewrite custom
    ## realine function which is in src/main/scan.c "do_readln"
    
    #  !! NOTE !!  This REVERSES WORD ORDER for easier subsetting to last words
    the_words <- tolower(rev(stringi::stri_extract_all_boundaries(the_string)[[1]]))[1:4]
    the_words <- the_words[!is.na(the_words)]
    
    
        # start with largest ngram and back-off until match is found
        # i = num *complete* words = current ngram length. Lookup will be i + 1 ngram.
        i <- sum(stri_count_charclass(the_words, "[[:space:][:punct:]]")) 
        # j, add a word to lookup phrase but don't increase ngram 
        # if last word is incomplete
        j <- ifelse(sum(stri_count_words(the_words)) > i, 1, 0) 
        i <- ifelse(i > 3, 3, i) # restrict to a max of 4gram lookup
        ngrams <- as.numeric(NULL)
        while(length(ngrams) == 0 & i >= 0){
            the_phrase <- paste0("^", paste0(the_words[(i + j):1], collapse = ""))
            if(i == 0) the_phrase <- gsub("[[:space:][:punct:]]$", "", the_phrase)
            ngrams <- smoothed_ngrams[[i + 1]][grep(the_phrase, names(smoothed_ngrams[[i + 1]]))]
            i <- i - 1
        }
        # if(length(ngrams) != 0){
        #     # We've got some hits, but let's see if there are any high potential
        #     # unobserved ngrams to add. Since we are only going as high as 4grams
        #     # code here
        # }
        
        next_words <- stri_extract_last_words(names(ngrams)[order(ngrams, decreasing = T)][1:4])
        next_words[!is.na(next_words)]  
}

