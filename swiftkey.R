# 

library(methods) #S4 objects in use
library(readtext)
library(quanteda)

# # library(tm)
# 
# # library(tokenizers)

## Create sample of data to practice with
# paths <- unzip("Coursera-SwiftKey.zip") # con/path to files in archive
# dlist <- lapply(paths[7:9], readLines)
# sample_size <- 5000
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
the_texts <- readtext("./sampled_EN/*.txt") # readtext("./final/en_US/*.txt")

text_names <- the_texts[["doc_id"]]

c_corpus <- corpus(texts(the_texts), groups = rep(1, length(text_names))) # collapse documents

my_tokens <- c_corpus %>% 
    tokens(remove_symbols = T,
           remove_punct = T,
           remove_separators = T, 
           remove_numbers = T,
           remove_twitter = T,
           remove_url = T) %>%
    tokens_tolower
rm(the_texts)


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
dfm_freq_plot(dfm_1gram)

# Plot coverege to determine how many words to keep (ie remove words that provide little value)
plot_coverage(dfm_1gram)

# The inflection point seems to be at about 10000 words, over 98% of word instances are covered
# Trimming beyond these words significantly reduced tokens and memory load. 

dfm_1gram <- dfm_trim(dfm_1gram, 
                    min_termfreq = 5000, 
                    termfreq_type = "rank")
freq_words <- names(topfeatures(dfm_1gram, 100))
my_tokens_reduced <- tokens_select(my_tokens, dfm_1gram@Dimnames$features, padding = F) 

# Quanteda pproach/alt to markov?:
collocations_df <- textstat_collocations(my_tokens, smoothing = 0.5, size = c(2:4))
library(data.table) 
collocations <- data.table(collocations) # half size of dframe, but operations same speed?
collocations[grepl("^in the ", collocations$collocation) & collocations$length == 4,]

dfm_2gram <- dfm(my_tokens_reduced, ngrams = 2, concatenator = " ")
# Only keep 2grams that are frequent, used more than twice in the corpus:
dfm_2gram_reduced <- dfm_trim(dfm_2gram, 
                              min_termfreq = 3, 
                              termfreq_type = "count")
tokens_2gram <-tokens(my_tokens_reduced, 
                      ngrams = 2, 
                      concatenator = " ", include_docvars = F)
tokens_2gram_reduced <- tokens_select(tokens_2gram, 
                                      dfm_2gram_reduced@Dimnames$features, 
                                      padding = F)

dfm_3gram <- dfm(my_tokens_reduced, ngrams = 3, concatenator = " ")
# Only keep 2grams that are frequent, used more than twice in the corpus:
dfm_3gram_reduced <- dfm_trim(dfm_3gram, 
                              min_termfreq = 3, 
                              termfreq_type = "count")
tokens_3gram <-tokens(my_tokens_reduced, 
                      ngrams = 3, 
                      concatenator = " ", include_docvars = F)
tokens_3gram_reduced <- tokens_select(tokens_3gram, 
                                      dfm_3gram_reduced@Dimnames$features, 
                                      padding = F)

tokens_4gram <- tokens(my_tokens_reduced, ngrams = 4, concatenator = " ")
test_dfm <- dfm(tokens_3gram)

tokens_3gram_reduced
# Apply Good-Turing discounting to smooth data
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

smoothed_list <- list(smoothed_1grams = gt_discount(my_tokens_reduced),
                      smoothed_2grams = gt_discount(tokens_2gram),
                      smoothed_3grams = gt_discount(tokens_3gram),
                      smoothed_4grams = gt_discount(tokens_4gram)
                      )
library(stringi)

smoothed_tokens <- smoothed_2grams

next_word_backoff_search <- function(the_string){
    
    library(stringi)
   
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
            ngrams <- smoothed_list[[i + 1]][grep(the_phrase, names(smoothed_list[[i + 1]]))]
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


num_tokens <- length(tokens_2gram[[1]])
unique_first_words <- unique(stri_extract_first_words(tokens_2gram[[1]]))
unique_last_words <- unique(stri_extract_last_words(tokens_2gram[[1]]))
matrix_2gram <- Matrix(0, 
                          nrow = length(unique_first_words),
                          ncol = length(unique_last_words),
                          dimnames = list(unique_first_words,
                                          unique_last_words
                              )
                          )
# This takes forever! need parallel. But parallel not working. can't figure out.
n <- 0
for(i in unique_first_words){
    ngrams <-  tokens_2gram[[1]][grep(paste0("^", i), tokens_2gram[[1]])]
    next_words <- stri_extract_last_words(ngrams)
    prop_table <- table(next_words)/length(next_words)
    for(j in names(prop_table)){
        matrix_2gram[i, j] <- prop_table[[j]]
    }
    n <- n + 1
    print(paste0(i, " ", 100 * n/length(unique_first_words), "%"))
}    

test_dgcmatrix <- Matrix(0, 
                         nrow = 5, 
                         ncol = 5, 
                         dimnames = list(unique_first_words[1:5], unique_last_words[1:5]))

dfm_1gram[["jefferson"]]
library(foreach)
library(parallel)
library(doParallel)
library(plyr)
#library(future)

no_cores <- detectCores() - 1
cl <- parallel::makeCluster(no_cores, verbose = T)
aaply(.data = test_dgcmatrix, .margin = 1, .fun = function(x){
    x <- 1
}, .parallel = T, .paropts = .(.packages = "Matrix"))

?a_ply

registerDoParallel(cl)
foreach(i = seq_len(nrow(test_dgcmatrix))) %dopar% {
    test_dgcmatrix[i,1] <- 1
}

new_matrix <- foreach(i = 1:nrow(test_dgcmatrix), 
                      .packages = 'Matrix', 
                      .combine=rbind, 
                      .inorder = T,
                      .final = Matrix) %dopar% {
    the_row <- test_dgcmatrix[i,]/sum(test_dgcmatrix[i,])
    the_row[is.na(the_row)] <- 0
    the_row
                                      
                      }

new_matrix <- foreach(i = unique_first_words[1:100], 
                      .packages = c("Matrix", "stringi"),
                      .export = c("tokens_2gram", "next_words"),
                      .combine = rbind, 
                      .inorder = T,
                      .final = Matrix) %dopar% {
                          ngrams <- tokens_2gram[[1]][grep(paste0("^", i), tokens_2gram[[1]])]
                          next_words <- stri_extract_last_words(ngrams)
                          prop_table <- table(next_words)/length(next_words)
                          # for(j in names(prop_table)){
                          #     matrix_2gram[i, j] <- prop_table[[j]]
                          # }
                      }

stopCluster(cl)
#cl <- future::makeClusterPSOCK(1, verbose = T)

# set.seed(42)
# textplot_wordcloud(dfm_1gram, min_count = 6, random_order = FALSE,
#                    rotation = .25, 
#                    color = RColorBrewer::brewer.pal(8,"Dark2"))
# 
# 
# 
# 




# # 1. What are the distributions of word frequencies?
# dfm_freq_plot(dfm_1gram)
# # 2. What are the frequencies of 2- and 3-grams?
# dfm_freq_plot(dfm_2gram)
# dfm_freq_plot(dfm_3gram)
# # 3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? This is "coverage".



# 
# # 4. How do you evaluate how many of the words come from foreign languages
# 
# ## Get English words
# library(qdapDictionaries)
# data("GradyAugmented")
# english <- readLines("https://raw.githubusercontent.com/wooorm/dictionaries/master/dictionaries/en-US/index.dic")
# english <- gsub("\\/[[:alpha:]]+","",english)
# library(stringi)
# english <- stri_trans_general(english, "lower")
# english <- stri_trans_general(english, "latin-ascii")
# 
# not_english <- setdiff(dfm_1gram@Dimnames$features, c(english, GradyAugmented))
# 
# ## Get words from non-english latin-based languages (examples) 
# 
# german <- readLines("https://raw.githubusercontent.com/wooorm/dictionaries/master/dictionaries/de/index.dic")
# german <- german[16:length(german)]
# german <- gsub("\\/[[:alpha:]]+","",german)
# german <- stri_trans_general(german, "lower")
# german <- stri_trans_general(german, "latin-ascii")
# french <- readLines("https://raw.githubusercontent.com/wooorm/dictionaries/master/dictionaries/fr/index.dic")
# french <- french[88:length(french)]
# french <- gsub("\\/[[:alpha:]]+","", french)
# french <- stri_trans_general(french, "lower")
# french <- stri_trans_general(french, "latin-ascii")
# 
# 
# maybe_german <- not_english[not_english %in% german]
# maybe_french <- not_english[not_english %in% french]


# 5. Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

## not sure about increasing coverage, but maybe make it more efficient by first prioritizing words that appear in ngrams and then throwing out words that appear past the inflection point in the coverage plot

## from wikipedia, https://en.wikipedia.org/wiki/N-gram

# "An issue when using n-gram language models are out-of-vocabulary (OOV) words. They are encountered in computational linguistics and natural language processing when the input includes words which were not present in a system's dictionary or database during its preparation. By default, when a language model is estimated, the entire observed vocabulary is used. In some cases, it may be necessary to estimate the language model with a specific fixed vocabulary. In such a scenario, the n-grams in the corpus that contain an out-of-vocabulary word are ignored. The n-gram probabilities are smoothed over all the words in the vocabulary even if they were not observed.[7]
# 
# Nonetheless, it is essential in some cases to explicitly model the probability of out-of-vocabulary words by introducing a special token (e.g. <unk>) into the vocabulary. Out-of-vocabulary words in the corpus are effectively replaced with this special <unk> token before n-grams counts are cumulated. With this option, it is possible to estimate the transition probabilities of n-grams involving out-of-vocabulary words.[8]"

# -------


# 1. Build basic n-gram model - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.

##https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf

# Build markov chain transition matrices

# library(plyr)
# splits <- laply(stri_split_fixed(dfm_2gram@Dimnames$features, " "), function(x) x)
# numrow <- length(unique(splits[,1]))
# numcol <- length(unique(splits[,2]))
# the_matrix <- as.matrix(rep(NA_real_, numrow * numcol), 
#                         nrow = numrow, 
#                         byrow = T)
# for(i in 1:numrow){
#     for(j in 1:numcol){
#         the_matrix[i,j] <- 0
#     }
# }

# Create a sparse feature co-occurrence matrix, measuring co-occurrences of features within a window of words, only looking ahead (ordered = T).

cf_matrix <- fcm(my_tokens, 
                 context = "window", 
                 window = 3, 
                 count = "weighted", 
                 ordered = T, 
                 tri = F)

# Given a word, find the next 3 most likely words


ret_next_words <- function(the_string = NA){
    
    the_string <- tolower(readline(paste0("Enter a phrase: ", the_string)))
    ## HUGE NOTE! readline() strips trailing whitespace so this doesn't quite
    ## work the way I want yet. For now always terminate with a punctuation
    ## char. Possible shiny app won't do this OR Ill have to rewrite custome
    ## realine function which is in src/main/scan.c "do_readln"
    
    the_words <- rev(stringi::stri_extract_all_boundaries(the_string)[[1]])
    the_words[2] <- gsub("[[:punct:][:space:]]", "", the_words[2])
    the_words[3] <- gsub("[[:punct:][:space:]]", "", the_words[3])

    last_word_s <- if (grepl("[[:punct:][:space:]]", the_words[1])){
        # last word ends in puntuation, therefore complete
        if(gsub("[[:punct:][:space:]]", "", the_words[1]) %in% 
           names(cf_matrix@margin)) {
            # last word is a complete entry & prefect match in matrix,
            # therefore, return the only word, removing punct and space
            gsub("[[:punct:][:space:]]", "", the_words[1])
        } else {
            # last word is a complete entry BUT not in the matrix
            NULL
        }
    } else if (length(x <-
                      grep(paste0(
                          "^", gsub("[[:punct:][:space:]]", "", the_words[1])
                      ), names(cf_matrix@margin), value = T)) > 0) {
        # no punct or space yet, word may be incomplete, return partial matches
        x
    } else {
        # no word entered
        NULL
    }
    
    penultimate_word <- if (the_words[2] %in% names(cf_matrix@margin)) {
        the_words[2] #prefect match, return the only word
    } else {
        NULL # there isn't a penultimate word OR no match, return <UNK>
    }
    
    antepenultimate_word <-
        if (the_words[3] %in% names(cf_matrix@margin)) {
            the_words[3] #prefect match, return the only word
        } else {
            NULL # there isn't a antepenultimate word OR no match, return <UNK>
        }
    
    next_word_s <- if(is.null(c(penultimate_word,
                                 antepenultimate_word))){
        # potenially have the last word, only
        if(length(last_word_s) > 1){
            # several partial matches on last word, pick most frequent
            rownames(cf_matrix[last_word_s, ])[order(rowSums(cf_matrix[last_word_s, ]), 
                                                     decreasing = T)][1:4]
        } else if(length(last_word_s) == 1){
            # last word is a perfect match, search for next word
            word_scores <-
                colSums(cf_matrix[c(last_word_s), ])
            names(word_scores[order(word_scores, decreasing = T)][1:4])
        } else {
            # all words missing, sample from 'frequent words' for next word
            # consider sampling for stopwords() instead
            sample(freq_words, 4, replace = F)
        }
    } else {
        # at least one full word matches
        if(length(last_word_s) > 1) {
            # have a at least one full word and partial matches for last word
            # subset by the full word(s), pick most frequent partial matches
            word_scores <-
                colSums(cf_matrix[c(penultimate_word, antepenultimate_word),
                                  names(cf_matrix@margin) %in% last_word_s])
            names(word_scores[order(word_scores, decreasing = T)])[1:4]
        } else {
            # last word is either a match or NULL.
            # Either way we have at least one full word, therefore
            # search matrix by the full word(s)
            word_scores <-
                colSums(cf_matrix[c(last_word_s, penultimate_word, antepenultimate_word), ])
            names(word_scores[order(word_scores, decreasing = T)][1:4])
            }
    }
                       
        
    next_word_s <- next_word_s[!is.na(next_word_s)]
    print("Thanks")
    print(paste0("The next most likely words are: ", paste0(next_word_s, collapse = ", ")))
 
}

the_string = ""
while(the_string != "q"){
    ret_next_words(the_string)
}
# 
# 1. How can you efficiently store an n-gram model (think Markov Chains)?
# reduce it to a matrix
#     2. How can you use the knowledge about word frequencies to make your model smaller and more efficient?
# x % of most freqeunt words capture y% of cases, so toss out 1-x of (infrequent) words
# 3.    How many parameters do you need (i.e. how big is n in your n-gram model)?
# so far, only 1 gram using co-occurrence matrix
# 4.     Can you think of simple ways to "smooth" the probabilities (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?
#   5. How do you evaluate whether your model is any good?
# validation on a test set? random phrases, each word fed, prediction compared to actual, tabluated by 0, 1, 2 and 3 gram effect on next word prediction.
#  6. How can you use backoff models to estimate the probability of unobserved n-grams?
# interesting, q asking to predict prob of n-grams not in corpus.


library(markovchain) 
sequence <- c("a", "b", "a", "a", "a", "a", "b", "a", "b", "a", 
              "b", "a", "a", "b", "b", "b", "a")
mcFit <- markovchainFit(data=sequence)