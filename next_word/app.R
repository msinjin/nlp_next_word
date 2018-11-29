#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(dplyr)
library(shinycssloaders)

columns <- c("position","word", "score")

ui <- fluidPage(
    
    # Header stuff ----
    
    # Javascript to keep focus in textinput box, paired with observeEvent in server
    tags$head(
        tags$script('Shiny.addCustomMessageHandler("refocus",
                function(NULL) {
                document.getElementById("the_string").focus();
                });
                ')
        # tags$script('(document).keyup(function(event) {
        #     if (event.key == "Enter")) {
        #         $("#goButton").click();
        #     }
        # });
        # ')
        ),    
    
    # Page layout ----
    
    titlePanel("Predictive Text App", "Predictive Text"),
    
    h5("Predictive text algorithm demonstration"),
    hr(),
    
    splitLayout(
        # set text to 'wrap' because default is to span the whole page:
        cellArgs = list(style='white-space: normal;'),
        verticalLayout(
            h4("Start typing!"),
            textAreaInput(inputId = "the_string",
                          width = "450px",
                          height = "100px",
                          resize = "vertical", 
                          placeholder = 'Type or paste some text here. You can use the dymanmic word prediction buttons (below) as shortcuts. Click the "Batch/Process" button to see how well the algorithm performed.',
                          label = "",
                      value = ""), 
            uiOutput(outputId = "prediction_buttons"),
            br(),
            actionButton("process","Batch/Process"),
            br(),
           wellPanel(
               h4("Your input:"),
               htmlOutput("input_text")
           )
        ),
        
        verticalLayout(
            wellPanel(
                h4("Word prediction success"),
            # DT::dataTableOutput(outputId = "word_scores"),
            #htmlOutput("html_text", inline = T),
            plotOutput("the_plot") ,
            br(),
            textOutput("percent_predicted")
        )
        )
    )
    
)


server <- function(input, output, session) {
    
    library(data.table)
    
    # observeEvent(input$process, {
    #     withProgress(message = "Parsing your text", 
    #                  detail = "Visuals are on the way...", 
    #                  value = 0, {
    #                      for (i in 1:15) {
    #                          incProgress(1/15)
    #                          Sys.sleep(0.25)
    #                      }
    #                  })
    # })
    
    # Clear out word score from prior run
    if (exists("word_scores", envir = .GlobalEnv)) {
        rm(word_scores, envir = .GlobalEnv)
    }
   
    observeEvent(input$the_string, {
        output$input_text <- renderText({
            if(input$the_string == ""){
                paste0('<i>Waiting for you to type something ...</i>')
            } else {
                paste0('"',input$the_string,'"')
            }
            
        })
        })
    
    # output$html_text <- renderText({
    #     input$the_string
    # })
    
    # Run Next word backoff search
   the_words <- reactive({
       # Find next words base on last word.
       input$the_string
       next_word <- next_word_backoff_search(input$the_string)
       # Now determine if the previous guess was successful
       # if(grepl("[[:space:][:punct:]]$", input$the_string)){
       #     the_word <- stri_extract_last_words(input$the_string)
       #     the_score <- min(which(the_word == previous_guess))
       #     the_score <- ifelse(is.infinite(the_score), 0, the_score)
       #     
       #     scored_string <- rbindlist(list(scored_string, 
       #                            data.table(position = stri_count_boundaries(input$the_string), 
       #                              word = the_word, 
       #                              score = the_score)))
       #     
       # }
       # 
       # previous_guess <- next_word
       assign("previous_guess", next_word, envir = .GlobalEnv)
       next_word
   })
   
   
   # ------
   # This section saves results to a table
   
   # create a data frame called word_scores
   # save_word_scores <- function(data) {
   #     data <- as.data.frame(t(data))
   #     if (exists("word_scores")) {
   #         word_scores <<- rbind(word_scores, data)
   #     } else {
   #         word_scores <<- data
   #     }
   # }
   # 
   loadData <- function() {
       if (exists("word_scores")) {
           word_scores
       }
   }
   
   # Whenever a field is filled, aggregate all form data
   #formData is a reactive function
   formData <- reactive({
       #data <- sapply(columns, function(x) input[[x]])
       position <- stri_count_boundaries(input$the_string)
       word <- stri_extract_last_words(input$the_string)
       score <- min(which(word == previous_guess))
       score <- ifelse(is.infinite(score), 0, score)
       data <- c(position,word,score)
       data
   })
   
   # When the process button is clicked, save the form data
   observeEvent(input$process, {
       progress <- Progress$new(session)
       on.exit(progress$close())
       progress$set(message = "Calculation in progress",
                    detail = "Visuals are on the way...")
       word_scores <- data.table(position = integer(), 
                               word = character(),
                               boundary = character(),
                               score = integer())
       the_string <- stri_extract_all_words(input$the_string)[[1]]
       the_boundaries <- stri_extract_all_boundaries(input$the_string)[[1]]
       word_scores <- rbindlist(list(word_scores, data.table(position = 1, 
                                                             word = the_string[1],
                                                             boundary = the_boundaries[1],
                                                             score = NA)))
       for(i in 1:(length(the_string) - 1)){
           progress$set(value = i)
           the_ngram <- paste0(the_string[1:i], " ", collapse = " ")
           the_guesses <- next_word_backoff_search(the_ngram)
           the_score <- min(which(the_string[i + 1] == the_guesses))
           the_score <- ifelse(is.infinite(the_score), 0, the_score)
           word_scores <- rbindlist(list(word_scores, data.table(position = i + 1, 
                                                    word = the_string[i + 1],
                                                    boundary = the_boundaries[i + 1],
                                                    score = the_score)))
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
           the_html <- NULL #paste0('<word_1 style="color:#2636EA;font-weight:bold;"><word_2 style="color:#535EEE;font-weight:bold;"><word_3 style="color: #7C86F2;font-weight:bold;"><word_4 style="color:#929AF4;font-weight:bold;">')
           for(i in 1:nrow(word_scores)){
               if(word_scores[i, "score"] == 0 | is.na(word_scores[i, "score"])){
                   the_html <- paste0(the_html, word_scores[i, "boundary"])
               } else if(word_scores[i, "score"] == 1){
                   the_html <- paste0(the_html,
                                      '<word_1 style="color:#2636EA;font-weight:bold;">', 
                                      word_scores[i, "boundary"], 
                                      "</word_1>", 
                                      collapse = "")
               } else if(word_scores[i, "score"] == 2){
                   the_html <- paste0(the_html,
                                      '<word_2 style="color:#535EEE;font-weight:bold;">', 
                                      word_scores[i, "boundary"], 
                                      "</word_2>", 
                                      collapse = "")
               } else if(word_scores[i, "score"] == 3){
                   the_html <- paste0(the_html,
                                      '<word_3 style="color: #7C86F2;font-weight:bold;">', 
                                      word_scores[i, "boundary"], 
                                      "</word_3>", 
                                      collapse = "")
               } else if(word_scores[i, "score"] == 4){
                   the_html <- paste0(the_html,
                                      '<word_4 style="color:#929AF4;font-weight:bold;">', 
                                      word_scores[i, "boundary"], 
                                      "</word_4>", 
                                      collapse = "")
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
               scale_fill_manual(values = c("0" = "darkgrey", 
                                            "1" = "#2636EA", 
                                            "2" ="#535EEE", 
                                            "3" = "#7C86F2", 
                                            "4" = "#929AF4")) +
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
           paste0("The word prediction model successfully predicted the next word ", percent_pred, "% of the time.")
       })
   })
   
  
    
   # ----- 
    
   output$prediction_buttons <- renderUI({
       splitLayout(actionButton("word_1", 
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
                                style = "color: #929AF4; font-weight :bold"))
   })
   
   # Monitor text buttons ----
   
   observeEvent(input$word_1,{
       # determine if last word is complete, otherwise replace it
       if(input$the_string == ""){
           the_string <- ""
       } else if(grepl("[[:space:][:punct:]]$", input$the_string)){
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
       
       session$sendCustomMessage("refocus",list(NULL))
   })
   
   observeEvent(input$word_2,{
       # determine if last word is complete, otherwise replace it
       if(input$the_string == ""){
           the_string <- input$word_2
       } else if(grepl("[[:space:][:punct:]]$", input$the_string)){
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
       session$sendCustomMessage("refocus",list(NULL))
   })
   
   observeEvent(input$word_3,{
       # determine if last word is complete, otherwise replace it
       if(grepl("[[:space:][:punct:]]$", input$the_string)){
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
       session$sendCustomMessage("refocus",list(NULL))
   })
   
   observeEvent(input$word_4,{
       # determine if last word is complete, otherwise replace it
       if(grepl("[[:space:][:punct:]]$", input$the_string)){
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
       session$sendCustomMessage("refocus",list(NULL))
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

