#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
library(shinythemes)

library(tidyverse) # metapackage with lots of helpful functions
library(tidytext)
library(janeaustenr)

library(data.table)
library(DT)
library(dplyr)

library(tm)
library(wordcloud)
library(stringr)
library(SnowballC)



question_list = list("Enter my own question" = 1,
                     "Data on potential risks factors" = 2,
                     "Transmission dynamics of the virus, including the basic reproductive number, incubation period, serial interval, modes of transmission and environmental factors" = 3,
                     "Severity of disease, including risk of fatality among symptomatic hospitalized patients, and high-risk patient groups" = 4,
                     "Susceptibility of populations" = 5,
                     "Public health mitigation measures that could be effective for control" = 6)

sub_question_list = list("Smoking, pre-existing pulmonary disease" = 2,
                         "Co-infections (determine whether co-existing respiratory/viral infections make the virus more transmissible or virulent) and other co-morbidities" = 3,
                         "Neonates and pregnant women" = 4,
                         "Socio-economic and behavioral factors to understand the economic impact of the virus and whether there were differences" = 5)

# Define UI for application that draws a histogram
ui <- fluidPage(
    shinythemes::themeSelector(),
    theme = shinytheme("flatly"),
    useShinyjs(),
    navbarPage("Coronavirus - Search engine",
               tabPanel(title = "Home", 
                        uiOutput("home")
               ),
               
               
               tabPanel("Data",
                        div( id ="Sidebar",sidebarPanel(
                            selectInput("question", label = "Wath do you want to know?", 
                                        choices = question_list,
                                        selected = 1
                            ),
                            
                            uiOutput("filtered_question"),
                            
                            actionButton("go", label = "Search"),
                            width = 12
                        )),
                        
                        
                        mainPanel(
                            actionButton("toggleSidebar", "Toggle sidebar"),
                            # Found Paper
                            DT::dataTableOutput("paper"),
                            width = 12
                        )
               ),
               tabPanel(title = "Plots", 
                        plotOutput("plots")
               )
    )
)



################################################################################

# **********************************************************

# Load the data once
if(!exists("meta_data")){
    temp <- tempfile()
    meta_data_path <- 'https://storage.googleapis.com/kaggle-data-sets/551982/1040416/compressed/metadata.csv.zip?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1586107068&Signature=k3DCr6q%2FIW7fMeAUMOZp90KG%2BgG%2FlsGLNfV%2BqHVPf%2FMEDgaUYbTYwrtpQ6HTFF9x0qRIV7Yb40ldsA0Its0B9w8Y%2B23c5tJZSFP7%2FHgMiLwEDpR4Ajk%2FRntvRLHZ06ff1%2FOmjtAw6fBUUA7JTk0RE5gNRCrbTFQBkv19OUz5jZWWP%2FNVvGG%2FmvC8ukRCX93VqW14EHE4kX8IcbYtWHAVWli5RYG2egB%2BDFrZXbPh09yGLhg3jh%2B8N4vt7IHst3t8hCKvcYQmvT4d3aK6%2FAVIUwdjGW1Rx8SpaAOinIjGzGqKUFHP1lAJAWylQUZ8rtLqE2pPfuQO8OU8seNz4aE0OQ%3D%3D&response-content-disposition=attachment%3B+filename%3Dmetadata.csv.zip'
    download.file(meta_data_path,temp)
    meta_data <- read.csv(unz(temp, "metadata.csv"))
}

# Fonction to search Paper
search_question <- function(question){
    
    ## Remove duplicated papers
    meta_data <- meta_data[!duplicated(meta_data$title),]
    
    data("stop_words")
    meta_data_cpl_abst <- meta_data[!is.na(meta_data$abstract), c("title", "doi", "pubmed_id", "abstract", "url")]
    
    token_cpl_abst <- meta_data_cpl_abst %>%
        select(title, doi, pubmed_id, abstract) %>%
        unnest_tokens(word, abstract, token = "ngrams", n = 1) %>%
        filter(!((word %in% stop_words$word))) %>%
        filter(!((word == "unknown")))
    
    ## Extract stemming 
    token_cpl_abst <- token_cpl_abst %>%
        mutate_at("word", funs(wordStem((.), language="en")))
    ##
    data("stop_words")
    
    
    question_tbl <- tibble(title = "question1",
                           doi = "doi_question1",
                           pubmed_id = 0,
                           abstract = question)
    ## Tokenize the question
    token_question <- question_tbl %>%
        unnest_tokens(word, abstract, token = "ngrams", n = 1) %>%
        filter(!((word %in% stop_words$word)))
    ## Extract stemming 
    token_question <- token_question %>%
        mutate_at("word", funs(wordStem((.), language="en")))
    
    token_cpl_abst_splited <- split(token_cpl_abst, (seq(nrow(token_cpl_abst))-1) %/% 50000) 
    
    similarities <- lapply(token_cpl_abst_splited, function(df, token_question) {
        token_cpl_abst_tmp <- rbind(df, token_question)
        tmp_sim <- token_cpl_abst_tmp %>%
            dplyr::count(title, word) %>%
            ungroup() %>%
            pairwise_similarity(title, word, n) %>%
            filter(item1 == "question1")
        return(tmp_sim)
    }, token_question = token_question)
    
    similarities_df <- as_tibble(Reduce("rbind", similarities))
    
    combined <- sort(union(levels(meta_data$title), levels(similarities_df$item2)))
    similarities_abst <- left_join(mutate(meta_data, title=factor(title, levels=combined)),
                                   mutate(similarities_df, item2=factor(item2, levels=combined)),
                                   by = c("title" = "item2")) %>%
        arrange(desc(similarity))
    
    similarities_abst
}
################################################################################

# Define server 
server <- function(input, output) {
    
    output$home <- renderUI(
        tags$div( 
            tags$div(
                style="background-image: url('https://cdn.pixabay.com/photo/2020/02/09/16/23/coronavirus-4833754__340.jpg'); 
                   background-repeat: no-repeat;
                   background-size: 100% 100%",
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$h1("COVID-19 Open Research Dataset Challenge (CORD-19)"),
                tags$p("")
            ),
            tags$div(
                style="text-align: center;",
                tags$p("In response to the COVID-19 pandemic,"),
                tags$p("the White House and a coalition of leading research groups"),
                tags$p("have prepared the COVID-19 Open Research Dataset (CORD-19). "),
                tags$p("This dataset is a resource of over 29,000 scholarly articles,"),
                tags$p("including over 13,000 with full text, about COVID-19, SARS-CoV-2, and related coronaviruses."),
                tags$br(),
                tags$p("This platform offers a search engine based on text and data mining and exploration,"),
                tags$p("that can help the medical community develop answers to high priority scientific questions.")
            ),
            tags$div(
                style="text-align: center;",
                tags$br(),
                tags$hr(),
            )
        )
    )
    
    observeEvent(input$toggleSidebar, {
        shinyjs::toggle(id = "Sidebar")
    })
    
    question <- eventReactive( input$go, {
        
        if (input$question == 1){
            if (is.null(input$user_question)){
                return()
            }
            input$user_question
        }
        else if (input$question == 2){
            names(which(sub_question_list == input$sub_question))
        }
        else if (input$question > 2){
            names(which(question_list == input$question))
        }
    })
    
    output$filtered_question <- renderUI(
        if (input$question == 1){
            textInput("user_question", "Please enter your question", "")
        }
        else if (input$question == 2){
            selectInput("sub_question", label = "Select Sub-question", 
                        choices = sub_question_list,
                        selected = 1
            )
        }
    )
    
    output$paper <- DT::renderDataTable({
        if (question() != ""){
            answer <- search_question(question())
            data <- answer %>%
                dplyr::select(similarity, title, url, abstract) %>%
                dplyr::mutate(url = paste0("<a href='", url, "'target='_blank'>","link to paper","</a>"))
        }
    }, escape = FALSE, options = list(searching = FALSE))
    
    output$plots <- renderPlot({
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)