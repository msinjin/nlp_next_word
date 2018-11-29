
library(readtext)
library(quanteda)

## Create train and test subsets of data
# paths <- unzip("Coursera-SwiftKey.zip") # con/path to files in archive
# dlist <- lapply(paths[7:9], readLines, skipNul = T)
# train_size <- 50000 # how many lines to extract from each document for training
# test_size <- 1000
# train_index <- lapply(dlist, function(x){
#     sample(1:length(x), train_size, replace = F)
# })
# the_names <- c("blogs", "news", "twitter")
# set.seed(42)
# train_sample <- list()
# test_sample <- list()
# for(i in 1:length(train_index)){
#     train_sample[[the_names[i]]] <- dlist[[i]][train_index[[i]]]
#     test_sample[[the_names[i]]] <- dlist[[i]][-train_index[[i]]]
#     test_sample[[the_names[i]]] <- test_sample[[the_names[i]]][sample(1:length(test_sample[[the_names[i]]]), test_size, replace = F)]
# }
# for(i in the_names){
#     write(train_sample[[i]], file = paste0("./corpus_train_EN/", i, ".txt"))
#     write(test_sample[[i]], file = paste0("./corpus_test_EN/", i, ".txt"))
# }
# rm(list = ls())

qopts <- quanteda_options()
quanteda_options(threads = 3)


texts_train <- readtext("./corpus_train_EN/*.txt") # readtext("./final/en_US/*.txt") 
corpus_train <- corpus(texts(texts_train)) 
# corpus_train <- corpus(texts(corpus_train, groups = rep(1, ndoc(corpus_train)))) # collapse documents

texts_test <- readtext("./corpus_test_EN/*.txt") # readtext("./final/en_US/*.txt") 
corpus_test <- corpus(texts(texts_test)) 
# corpus_test <- corpus(texts(corpus_test, groups = rep(1, ndoc(corpus_test)))) # collapse documents
rm(texts_train, texts_test)

tokens_train <- corpus_train %>% 
    tokens(remove_symbols = T,
           remove_punct = T,
           remove_separators = T, 
           remove_numbers = T,
           remove_twitter = T,
           remove_url = T) %>%
    tokens_tolower

tokens_test <- corpus_test %>% 
    tokens(remove_symbols = T,
           remove_punct = T,
           remove_separators = T, 
           remove_numbers = T,
           remove_twitter = T,
           remove_url = T,
           ngrams = c(2:5),
           concatenator = " ") %>%
    tokens_tolower

rm(corpus_train, corpus_test)

# Strip infrequent tokens out of my_tokens
# reduces sparcity and massively reduces matrix memory requirements
# 
# dfm_freq_plot <- function(dfm){
#     library(ggplot2)
#     features_dfm <- textstat_frequency(dfm, 100)
#     features_dfm$stopword <- features_dfm$feature %in% stopwords()
#     features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))
#     ggplot(features_dfm, aes(x = feature, y = frequency, color = stopword)) +
#         geom_point() +
#         theme(axis.text.x = element_text(angle = 90, hjust = 1), 
#               panel.grid.major = element_blank(),
#               legend.position="none")
# }
# plot_coverage <- function(dfm) {
#     library(ggplot2)
#     features_dfm <- textstat_frequency(dfm)
#     features_dfm$feature <-
#         with(features_dfm, reorder(feature, frequency))
#     
#     per_word_instance <- NA_real_
#     unique_words <- NA_integer_
#     total_feature_frequency <- sum(features_dfm$frequency)
#     for (i in 1:100) {
#         unique_words[i] <- round(length(features_dfm$feature) * i / 100, 0)
#         the_words <-
#             as.character(features_dfm$feature[1:unique_words[i]])
#         freq_of_the_words <-
#             sum(features_dfm[1:unique_words[i], "frequency"])
#         per_word_instance[i] <-
#             freq_of_the_words / total_feature_frequency * 100
#     }
#     
#     ggplot(
#         data.frame(unique_words, per_word_instance),
#         aes(x = unique_words, y = per_word_instance)
#     ) +
#         geom_line() +
#         ggtitle("Coverage")
# }

dfm_1gram <- dfm(tokens_train)

# Plot coverage to determine how many words to keep (ie remove words that provide little value)
# dfm_freq_plot(dfm_1gram)
# plot_coverage(dfm_1gram)
# The inflection point seems to be at about 10000 words, over 98% of word instances are covered
# Trimming beyond these words significantly reduced tokens and memory load. 

dfm_1gram <- dfm_trim(dfm_1gram, 
                    min_termfreq = 5000, 
                    termfreq_type = "rank")

frequent_words <- names(topfeatures(dfm_1gram, n = 50))

tokens_train <- tokens_select(tokens_train, dfm_1gram@Dimnames$features, padding = F) 

# Create token ngrams lists:
ngram_list <- list(ngram_1 = tokens_train)
# rm(tokens_train)

for(ngram in 2:5){
    the_tokens <- tokens(ngram_list[["ngram_1"]], ngrams = ngram, concatenator = " ")
    the_dfm <- dfm(the_tokens)
    the_dfm <- dfm_trim(the_dfm, min_termfreq = 2, termfreq_type = "count")
    ngram_list[[paste0("ngram_",ngram)]] <- tokens_select(the_tokens, the_dfm@Dimnames$features, padding = F)
}

rm(the_dfm, the_tokens)

# Make frequency tables and apply Good-Turing discounting to smooth data
gt_discount <- function(tokens, remove_singles = T){
    the_counts <- table(tokens[[1]])
    # remove tokens that only appear once
    if(remove_singles == T) the_counts <- the_counts[the_counts > 1] 
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

smoothed_ngrams <- list(list(blogs.txt = list(), news.txt = list(), twitter.txt = list()))
for(j in c("blogs.txt", "news.txt", "twitter.txt")){
    for(i in 1:5){
        smoothed_ngrams[[j]][[paste0("ngram_",i)]] <- gt_discount(ngram_list[[paste0("ngram_",i)]][j])    
    }
}

rm(ngram_list)

# Up to here just to get ngram frequency data, smoothed_ngrams, for model ----

# Create naive bayes model predicting which corpus new text should be matched against

nb_model <- textmodel_nb(dfm_1gram, dfm_1gram@Dimnames$docs)
# rm(dfm_1gram)

make_tokens <- function(the_string, which_corpus = NULL, remove_stopwords = F){
    
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
        tokens_tolower %>% 
        {if(remove_stopwords == T) tokens_remove(., stopwords()) else .} %>%
        .[[1]] %>% 
        rev(.) %>% 
        .[1:4] %>% 
        .[!is.na(.)]
    
    if(is.null(which_corpus)){
        which_corpus <- as.character(predict(nb_model, 
                                         newdata = dfm_select(dfm(the_tokens), 
                                                              dfm_1gram))[[1]])
    } 
    
    list(the_tokens = the_tokens, which_corpus = which_corpus)
}


next_word_backoff_search <- function(the_string, 
                                     max_gram = 3, 
                                     which_corpus = NULL,
                                     remove_stopwords = F){
   
    if(!grepl("[[:alnum:]]", the_string)){
        return(sample(frequent_words, 4, replace = F))
    }
    # max_gram is largest ngram that will be used for next work lookup
    # in the max_gram + 1 smoothed_ngrams table
    
    tokens_list <- make_tokens(the_string, 
                              remove_stopwords = remove_stopwords,
                              which_corpus = which_corpus)
    
    the_tokens <- tokens_list[["the_tokens"]]
    which_corpus <- tokens_list[["which_corpus"]]
    
    #  !! NOTE !!  This REVERSES WORD ORDER for easier subsetting to last words
    # the_words <- tolower(rev(stringi::stri_extract_all_boundaries(the_string)[[1]]))[1:4]
    # the_words <- the_words[!is.na(the_words)]
    # 
    
        # start with largest ngram and back-off until match is found
        # i = num *complete* words = current ngram length. Lookup will be i + 1 ngram.
        i <- length(the_tokens)
        # j, add a word to lookup phrase but don't increase ngram 
        # if last word is incomplete
        j <- ifelse(grepl("[[:space:][:punct:]]$", the_string), 0, 1)
        i <- ifelse(i > max_gram, max_gram, i) # restrict lookup
        ngrams <- as.numeric(NULL)
        while(length(ngrams) == 0 & i > 0){
            the_phrase <- paste0("^", paste0(the_tokens[i:1], collapse = " "))
            if(i - j == 0) the_phrase <- gsub("[[:space:][:punct:]]$", "", the_phrase)
            ngrams <- smoothed_ngrams[[which_corpus]][[i - j + 1]][
                stri_detect_regex(names(smoothed_ngrams[[which_corpus]][[i - j + 1]]), 
                                  the_phrase)] # faster than grep()
            i <- i - 1
        }
      
        
        if(length(ngrams > 0)){
            next_words <- stri_extract_last_words(names(ngrams)[order(ngrams, 
                                                                      decreasing = T)][1:4])    
        } else {
            next_words <- sample(frequent_words, 4, replace = F)
        }
        
        if(any(is.na(next_words))){
            next_words[is.na(next_words)] <- sample(frequent_words, 
                                                    length(next_words[is.na(next_words)]))     
        }
        
        next_words
}

next_word_rare_search <- function(the_string, max_gram = 3){
    # consider taking the rarest token in the string and 
    # finding ngrams that contain it
    the_tokens <- make_tokens(the_string, max_gram, remove_stopwords = T)
    
    rare_token <- names(smoothed_ngrams[[which_corpus]][[1]][the_tokens])[
        order(smoothed_ngrams[[which_corpus]][[1]][the_tokens],
              decreasing = F)][1]
    nearby_tokens <- tokens(texts(c(
            names(smoothed_ngrams[[which_corpus]][[3]][stri_detect_regex(names(smoothed_ngrams[[which_corpus]][[3]]), rare_token)]),
            names(smoothed_ngrams[[which_corpus]][[4]][stri_detect_regex(names(smoothed_ngrams[[which_corpus]][[4]]), rare_token)]),
            names(smoothed_ngrams[[which_corpus]][[5]][stri_detect_regex(names(smoothed_ngrams[[which_corpus]][[5]]), rare_token)])), 
            groups = 1)) # faster than grep()    

    nearby_tokens <- tokens_remove(nearby_tokens, the_tokens)
    next_words <- table(nearby_tokens[[1]])
    next_words <- next_words[order(next_words, decreasing = T)][1:4]
    names(next_words[!is.na(next_words)])
}

next_word_skip_search <- function(the_string, max_gram = 3){
    
    the_tokens <- make_tokens(the_string, max_gram)
    
    # # now consider the provided words out of sequence and match all n+1 grams, 
    # # return the extra word
    # i <- length(the_tokens) 
    # j <- ifelse(grepl("[[:space:][:punct:]]$", the_string), 0, 1)
    # i <- ifelse(i > max_gram, max_gram, i) # restrict to a max of 4gram lookup
    # skipgrams <- as.numeric(NULL)
    # while(length(skipgrams) == 0 & i >= 0){
    #     smoothed_ngrams[[which_corpus]][[i - j + 1]][the_tokens[i:1] %in% names(smoothed_ngrams[[which_corpus]][[i - j + 1]])]
    # }
    # the_rows <- NA_integer_
    # for(i in the_tokens){
    #     the_rows <- c(the_rows, grep(i, names(smoothed_ngrams[[which_corpus]][[5]])))
    # }
    # the_rows[duplicated(the_rows)]
    
    
    # if(length(ngrams) != 0){
    #     # We've got some hits, but let's see if there are any high potential
    #     # unobserved ngrams to add. Since we are only going as high as 4grams
    #     # code here
    # }
}

# Alternative model approach, Feature co-occurence matrix (quanteda)
# Create a sparse feature co-occurrence matrix, measuring co-occurrences of features within a window of words, only looking ahead (ordered = T).

feat_co_matrix <- fcm(tokens_train, 
                      context = "window", 
                      window = 5, 
                      count = "weighted", 
                      weights = c(5,4,3,2,1),
                      ordered = F, 
                      tri = F)
    
next_word_co_matrix_search <- function(the_string, max_gram = 3){

    the_tokens <- make_tokens(the_string, max_gram)
    the_tokens <- the_tokens[the_tokens %in% rownames(feat_co_matrix)]
    if(length(the_tokens) != 0){
        word_scores <- colSums(feat_co_matrix[the_tokens, ])
        next_words <- names(word_scores[order(word_scores, decreasing = T)][1:4])
    } else {
        next_words <- names(sample(frequent_words, 4, replace = F))
    }
    
    next_words
    
}

## Test performance and accuracy of next word models

library(lineprof)

validate_model <- function(max_gram = 3, fun = next_word_backoff_search){
    library(data.table)
    
    tokens_test_sample <- lapply(tokens_test, sample, size = 500, replace = F)
    length_corpus <- lapply(tokens_test_sample, length)
    which_corpus <- NULL
    for(i in names(length_corpus)){
        which_corpus <- c(which_corpus, rep(i, length_corpus[[i]]))
    }
    validation_dt <- data.table(corpus = which_corpus,
                                ngram = as.character(unlist(tokens_test_sample)), 
                                words = NA_integer_, 
                                score = NA_integer_)
    
        for(i in 1:nrow(validation_dt)){
            test_ngram <- validation_dt[i,ngram]
            num_words_test <- stri_count_boundaries(test_ngram)
            next_word_test <- stri_extract_last_words(test_ngram)
            the_string_test <- gsub(next_word_test, "", test_ngram)
            next_words <- fun(the_string_test, 
                              max_gram = max_gram, which_corpus = validation_dt[i,corpus])
            the_res <- which(grepl(next_word_test, next_words))
            if(sum(the_res) == 0){
                the_score <- 0
            } else if(1 %in% the_res){
                the_score <- 4
            } else if(2 %in% the_res){
                the_score <- 3
            } else if (3 %in% the_res){
                the_score <- 2
            } else {
                the_score <- 1
            }
            validation_dt[ngram == test_ngram, `:=`(max_gram = max_gram,
                                                    words = num_words_test,
                                                    next_word = next_word_test,
                                                    next_1 = next_words[1],
                                                    next_2 = next_words[2],
                                                    next_3 = next_words[3],
                                                    next_4 = next_words[4],
                                                    score = the_score)]
        }
    validation_dt
}




res <- validate_model(max_gram = 4, fun = next_word_backoff_search)
# validate_rare_model <- validate_model(max_gram = 4, fun = next_word_rare_search)
# validate_co_mat <- validate_model(max_gram = 4, fun = next_word_co_matrix_search)

for(i in 2:5){
    print(res[words == i & score > 0,.N] / res[words == i ,.N])
}



