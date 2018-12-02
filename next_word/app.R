#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Setup and load functions & constants ----

library(shiny)
library(data.table)
library(stringi)
library(ggplot2)

smoothed_ngrams <- readRDS("smoothed_ngrams.RDS")

frequent_words <-
    c(
        "the",
        "to",
        "and",
        "a",
        "of",
        "in",
        "i",
        "that",
        "for",
        "is",
        "it",
        "on",
        "you",
        "with",
        "was",
        "at",
        "this",
        "my",
        "as",
        "be"
    )

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


##### The Shiny app ----

ui <- fluidPage(
    # Header stuff ----
    
    # Javascript to keep focus in textinput box, paired with observeEvent in server
    tags$head(
        tags$script(
            'Shiny.addCustomMessageHandler("refocus",
            function(NULL) {
            document.getElementById("the_string").focus();
            });
            '
    )
    ),
    
    # Page layout ----
    
    titlePanel("Predictive Text App", "Predictive Text"),
    
    h5("Predictive text algorithm demonstration"),
    hr(),
    
    splitLayout(
        # set text to 'wrap' because default is to span the whole page:
        cellArgs = list(style = 'white-space: normal;'),
        verticalLayout(
            h4("Start typing!"),
            textAreaInput(
                inputId = "the_string",
                width = "450px",
                height = "100px",
                resize = "vertical",
                placeholder = 'Type or paste some text here. You can use the dymanmic word prediction buttons (below) as shortcuts. Click the "Batch/Process" button to see how well the algorithm performed.',
                label = "",
                value = ""
            ),
            uiOutput(outputId = "prediction_buttons"),
            br(),
            actionButton("process", "Batch/Process"),
            br(),
            wellPanel(h4("Your input:"),
                      htmlOutput("input_text"))
        ),
        
        verticalLayout(wellPanel(
            h4("Word prediction success"),
            # DT::dataTableOutput(outputId = "word_scores"),
            #htmlOutput("html_text", inline = T),
            plotOutput("the_plot") ,
            br(),
            textOutput("percent_predicted")
        ))
    )
    
    )


server <- function(input, output, session) {
    library(data.table)
    
    # Clear out word score from prior run
    if (exists("word_scores", envir = .GlobalEnv)) {
        rm(word_scores, envir = .GlobalEnv)
    }
    
    observeEvent(input$the_string, {
        output$input_text <- renderText({
            if (input$the_string == "") {
                paste0('<i>Waiting for you to type something ...</i>')
            } else {
                paste0('"', input$the_string, '"')
            }
        })
    })
    
    # Run Next word backoff search
    the_words <- reactive({
        # Find next words base on last word.
        input$the_string
        next_word <- next_word_backoff_search(input$the_string)
        assign("previous_guess", next_word, envir = .GlobalEnv)
        next_word
    })
    
    
    
    # This section saves results to a table
    loadData <- function() {
        if (exists("word_scores")) {
            word_scores
        }
    }
    
    # Whenever a field is filled, aggregate all form data
    #formData is a reactive function
    formData <- reactive({
        position <- stri_count_words(input$the_string)
        word <- stri_extract_last_words(input$the_string)
        score <- min(which(word == previous_guess))
        score <- ifelse(is.infinite(score), 0, score)
        data <- c(position, word, score)
        data
    })
    
    # When the process button is clicked, save the form data
    observeEvent(input$process, {
        progress <- Progress$new(session)
        on.exit(progress$close())
        progress$set(message = "Calculation in progress",
                     detail = "Visuals are on the way...")
        word_scores <- data.table(
            position = integer(),
            word = character(),
            boundary = character(),
            score = integer()
        )
        the_string <- stri_extract_all_words(input$the_string)[[1]]
        the_boundaries <-
            stri_extract_all_boundaries(gsub("\\n+", " ", input$the_string))[[1]]
        word_scores <-
            rbindlist(list(
                word_scores,
                data.table(
                    position = 1,
                    word = the_string[1],
                    boundary = the_boundaries[1],
                    score = NA
                )
            ))
        for (i in 1:(length(the_string) - 1)) {
            progress$set(value = i)
            the_ngram <-
                paste0(the_string[1:i], " ", collapse = " ")
            the_guesses <- next_word_backoff_search(the_ngram)
            the_score <-
                min(which(the_string[i + 1] == the_guesses))
            the_score <-
                ifelse(is.infinite(the_score), 0, the_score)
            word_scores <-
                rbindlist(list(
                    word_scores,
                    data.table(
                        position = i + 1,
                        word = the_string[i + 1],
                        boundary = the_boundaries[i + 1],
                        score = the_score
                    )
                ))
        }
        word_scores <<- word_scores
    })
    
    # Show the previous word_scores
    # (update with current response when save is clicked)
    output$word_scores <- DT::renderDataTable({
        input$process
        loadData()
    })
    
    
    
    observeEvent(input$process, {
        output$input_text <- renderUI({
            the_html <-
                NULL #paste0('<word_1 style="color:#2636EA;font-weight:bold;"><word_2 style="color:#535EEE;font-weight:bold;"><word_3 style="color: #7C86F2;font-weight:bold;"><word_4 style="color:#929AF4;font-weight:bold;">')
            for (i in 1:nrow(word_scores)) {
                if (word_scores[i, "score"] == 0 | is.na(word_scores[i, "score"])) {
                    the_html <- paste0(the_html, word_scores[i, "boundary"])
                } else if (word_scores[i, "score"] == 1) {
                    the_html <- paste0(
                        the_html,
                        '<word_1 style="color:#2636EA;font-weight:bold;">',
                        word_scores[i, "boundary"],
                        "</word_1>",
                        collapse = ""
                    )
                } else if (word_scores[i, "score"] == 2) {
                    the_html <- paste0(
                        the_html,
                        '<word_2 style="color:#535EEE;font-weight:bold;">',
                        word_scores[i, "boundary"],
                        "</word_2>",
                        collapse = ""
                    )
                } else if (word_scores[i, "score"] == 3) {
                    the_html <- paste0(
                        the_html,
                        '<word_3 style="color: #7C86F2;font-weight:bold;">',
                        word_scores[i, "boundary"],
                        "</word_3>",
                        collapse = ""
                    )
                } else if (word_scores[i, "score"] == 4) {
                    the_html <- paste0(
                        the_html,
                        '<word_4 style="color:#929AF4;font-weight:bold;">',
                        word_scores[i, "boundary"],
                        "</word_4>",
                        collapse = ""
                    )
                }
            }
            the_html <- paste0('"', the_html, '"')
            HTML(the_html)
        })
        
    })
    
    
    
    observeEvent(input$process, {
        output$the_plot <- renderPlot({
            word_scores[, success := ifelse(score > 0, "Success", "Failure")]
            library(ggplot2)
            ggplot(data = word_scores[!is.na(score)], aes(x = success, fill = as.factor(score))) +
                geom_bar(stat = "count") +
                scale_fill_manual(
                    values = c(
                        "0" = "darkgrey",
                        "1" = "#2636EA",
                        "2" = "#535EEE",
                        "3" = "#7C86F2",
                        "4" = "#929AF4"
                    )
                ) +
                xlab("Prediction") +
                ylab("Count") +
                theme(legend.position = "none")
        })
    })
    
    observeEvent(input$process, {
        output$percent_predicted <- renderText({
            denominator <- nrow(word_scores) - 1
            numerator <- nrow(word_scores[score > 0])
            percent_pred <- round(100 * numerator / denominator, 1)
            paste0(
                "The word prediction model successfully predicted the next word ",
                percent_pred,
                "% of the time."
            )
        })
    })
    
    # -----
    
    output$prediction_buttons <- renderUI({
        splitLayout(
            actionButton("word_1",
                         the_words()[1],
                         style = "color: #2636EA; font-weight :bold"),
            actionButton("word_2",
                         the_words()[2],
                         style = "color: #535EEE; font-weight:bold"),
            actionButton("word_3",
                         the_words()[3],
                         style = "color: #7C86F2; font-weight :bold"),
            actionButton("word_4",
                         the_words()[4],
                         style = "color: #929AF4; font-weight :bold")
        )
    })
    
    # Monitor text buttons ----
    
    observeEvent(input$word_1, {
        # determine if last word is complete, otherwise replace it
        if (input$the_string == "") {
            the_string <- ""
        } else if (grepl("[[:space:][:punct:]]$", input$the_string)) {
            the_string <- input$the_string
        } else {
            the_string <- gsub(stri_extract_last_boundaries(input$the_string),
                               "",
                               input$the_string)
        }
        updateTextInput(session,
                        "the_string",
                        value = paste(the_string,
                                      the_words()[1],
                                      sep = ""))
        
        session$sendCustomMessage("refocus", list(NULL))
    })
    
    observeEvent(input$word_2, {
        # determine if last word is complete, otherwise replace it
        if (input$the_string == "") {
            the_string <- input$word_2
        } else if (grepl("[[:space:][:punct:]]$", input$the_string)) {
            the_string <- input$the_string
        } else {
            the_string <- gsub(stri_extract_last_boundaries(input$the_string),
                               "",
                               input$the_string)
        }
        updateTextInput(session,
                        "the_string",
                        value = paste(the_string,
                                      the_words()[2],
                                      sep = ""))
        session$sendCustomMessage("refocus", list(NULL))
    })
    
    observeEvent(input$word_3, {
        # determine if last word is complete, otherwise replace it
        if (grepl("[[:space:][:punct:]]$", input$the_string)) {
            the_string <- input$the_string
        } else {
            the_string <- gsub(stri_extract_last_boundaries(input$the_string),
                               "",
                               input$the_string)
        }
        updateTextInput(session,
                        "the_string",
                        value = paste(the_string,
                                      the_words()[3],
                                      sep = ""))
        session$sendCustomMessage("refocus", list(NULL))
    })
    
    observeEvent(input$word_4, {
        # determine if last word is complete, otherwise replace it
        if (grepl("[[:space:][:punct:]]$", input$the_string)) {
            the_string <- input$the_string
        } else {
            the_string <- gsub(stri_extract_last_boundaries(input$the_string),
                               "",
                               input$the_string)
        }
        updateTextInput(session,
                        "the_string",
                        value = paste(the_string,
                                      the_words()[4],
                                      sep = ""))
        session$sendCustomMessage("refocus", list(NULL))
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
