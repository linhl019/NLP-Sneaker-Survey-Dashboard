#########################################################
# Initialization                                        #
#########################################################
library(shinyBS)
library(rintrojs)
library(shinydashboard)
library(shinyWidgets)
questions <- c("1.	How do you select your shoes? (looks, comfort, brand, functionality...)", 
               "2.	Where are you from?",
               "3.	What kind of physical activity do you typically do per week?",
               "4.	What is your gender?",
               "5.	What is your age?",
               "6.	What is favorite kind of shoe?",
               "7.  Overall")
first <- questions[1]

#########################################################
#Header code                                            #
#########################################################
header <- dashboardHeader(title = "SNEAKER - Team 7", titleWidth = 230, 
    dropdownMenu(type="messages", messageItem(from="Ariosto", message = "Last chance", href ="http://www.datacamp.com")),
    dropdownMenu(type="notifications", notificationItem(tex="check out datacamp", href="http://www.datacamp.com")),
    dropdownMenu(type="tasks", taskItem(text="look at datacamp progress", value=20))
)
#########################################################
#Sidebar code                                           #
#########################################################
sidebar <- dashboardSidebar(
    sidebarMenu(menuItem("OVERVIEW", tabName = "Exec", icon = icon("table")),
                menuItem("ANALYSIS", tabName = "Analysis", icon = icon("refresh")),
                menuItem("TOPICS MODEL", tabName = "Topics", icon = icon("bar-chart-o")),
                menuItem("PREDICTIVE", tabName = "Predictive", icon = icon("dashboard")),
                menuItem("SIMULATION", tabName = "Simulation", icon = icon("caret-square-right")),
                menuItem("INSIGHTS", tabName = "Business", icon = icon("lightbulb")),
                tags$head(tags$style(HTML(".skin-blue .main-sidebar {background-color:  grey;}"))) #color
                )
)
#########################################################
# Box availables                                        #
#########################################################
b_age <- valueBoxOutput("age", width=3)
b_gender <- valueBoxOutput("gender", width=3)
b_country <- valueBoxOutput("country", width=3)
b_intro1 <- infoBoxOutput("problem", width=9)
b_intro2 <- infoBoxOutput("text2",   width=12)
b_intro3 <- infoBoxOutput("senti",   width = 3)
b_intro4 <- infoBoxOutput("pred",    width = 3)
b_intro5 <- infoBoxOutput("pers",    width = 3)

b_question <- box(title = "Survey questions IO", status = "success", 
                  width = 10, height = 150, solidHeader = TRUE, 
                  selectInput(inputId = "question", 
                              label = "Select a question:", choices = questions))
b_frequencies <- box(title = "Most frequent words", width = 5,
                     status = "primary",  solidHeader = TRUE, collapsible = TRUE,
                     plotOutput("frequencies", height = 250))

b_wordcloud <- box(title = "Sentiment Word Cloud", width = 5,
                   status = "primary", solidHeader = TRUE, collapsible = TRUE,
                   plotOutput("wordcloud", height = 250))





b_lexicon <- box(title = "Sentiments by lexicon", width = 6,
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    plotOutput("lexicon", height = 500))

b_sentiments <- box(title = "Sentiment analysis", width = 10,
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    plotOutput("sentiment", height = 250))

b_network <- box(title = "Words Network", width = 10,
                 status = "primary", solidHeader = TRUE, collapsible = TRUE,
                 plotOutput("network", height = 500))

b_tfidf <- box(title = "Survey IT-IDF Analysis", width = 10,
               status = "warning", solidHeader = TRUE, collapsible = TRUE,
               plotOutput("tfidf", height = 500))               

b_choice_L <- box(title = "Latent Dirichlet Allocation (LDA)", status = "success", 
                  width = 5, height = 120, solidHeader = TRUE,
                  fluidRow(
                      column(
                          width = 12,
                          introBox(
                              bsButton("beta", 
                                       label = "BETA", 
                                       icon = icon("spinner"),
                                       style = "success"),
                              bsButton("gamma", 
                                       label = "GAMMA", 
                                       icon = icon("spinner", class = "spinner-box"), 
                                       style = "success")
                          )
                      )
                  )
)


b_beta <- box(title = "Topics Analysis",
              status = "warning", width = 10, solidHeader = TRUE, collapsible = TRUE,
              plotOutput("topic", height = 500))

#b_gamma <- box(title = "Topics Analysis (Gamma)",
# status = "warning", width = 12, solidHeader = TRUE, collapsible = TRUE,
# plotOutput("gamma", height = 500))

b_select_B_G <- box(title = "Survey questions IO", status = "success", 
                    width = 10, height = 150, solidHeader = TRUE, 
                    selectInput(inputId = "Choice", 
                                label = "Select:", choices = questions))

b_predictive <- box(title = "Prediction Accuracy",
                  status = "warning", width = 10, solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("predictive", height = 500))


b_simulation <- box(
    title = "Inputs", status = "warning",
    br(),
    textInput("selection", "How do you select your shoes? (looks, comfort, brand, functionality...)?"),
    textInput("ctry", "Where are you from?"),
    textInput("activity", "What kind of physical activity do you typically do per week?"),
    selectInput("gender", "Gender:",
                c("Female" = "female",
                  "Male" = "male")),
    sliderInput("age", "Age: ", 17, 75, 25),
    textInput("fav", "Favorite kind of shoes"),
    
    actionButton("action",  
                 label = "Enter" 
                 #icon = icon("spinner", class = "spinner-box"), 
                 #style = "success"
    )
)


b_slider <- box(title = "Training Size", status = "success", 
                width = 6
                , height = 150, solidHeader = TRUE, 
                sliderInput("slide",
                            "Size in %:",
                            min = 30,
                            max = 90,
                            step = 1,         
                            value = 35))

b_accuracy <- infoBoxOutput("accu", width = 5)
b_words <- tableOutput("words")
b_prediction <- infoBoxOutput("prediction", width = 12)

#b_X <- valueBoxOutput("Bin1")
b_XX <- valueBoxOutput("Bin2", width=3)
b_XXX <- valueBoxOutput("Bin3", width=3)
b_XXXX <- valueBoxOutput("Bin4", width=3)

b_in1 <- infoBoxOutput("in1", width = 12)
b_in2 <- infoBoxOutput("in2", width = 12)
b_in3 <- infoBoxOutput("in3", width = 6)
b_in4 <- infoBoxOutput("in4", width = 6)
b_in5 <- infoBoxOutput("in5", width = 3)

##########################################################
# Panels structure                                       #
##########################################################
#Panel Intro
intro_row1 <- fluidRow(b_age, b_gender, b_country)
intro_row2 <- fluidRow(b_intro1)
intro_row3 <- fluidRow(b_intro2)
intro_row4 <- fluidRow(b_intro3, b_intro4, b_intro5)

#Panel Preliminary
pre_row1 <- fluidRow(b_question)
pre_row2 <- fluidRow(b_frequencies, b_wordcloud)
pre_row3 <- fluidRow(b_sentiments)
pre_row4 <- fluidRow(b_network)
pre_row5 <- fluidRow(b_tfidf)

#Panel topics
des_row1 <- fluidRow(b_choice_L)
des_row2 <- fluidRow(b_beta)
des_row3 <- fluidRow(b_simulation, b_prediction)
#des_row3 <- fluidRow(b_gamma)

#Panel Predictive
pred_row1 <- fluidRow(b_predictive)
pred_row2 <- fluidRow(b_slider, b_accuracy)
pred_row3 <- fluidRow(b_words)

#Panel Insights
in_row1 <- fluidRow(b_XX, b_XXX, b_XXXX)
in_row2 <- fluidRow(b_in1)
in_row3 <- fluidRow(b_in2)
in_row4 <- fluidRow(b_in3, b_in4, b_in5)

body <- dashboardBody(
    tabItems(tabItem(tabName = "Exec", intro_row1, intro_row2, intro_row3, intro_row4),
             tabItem(tabName = "Analysis", pre_row1, pre_row2, pre_row3, pre_row4, pre_row5),
             tabItem(tabName = "Topics", des_row1, des_row2),
             tabItem(tabName = "Predictive", pred_row2, pred_row1, pred_row3),
             tabItem(tabName = "Simulation", des_row3),
             tabItem(tabName = "Business", in_row1, in_row2, in_row3, in_row4)))
##########################################################

#Main code
UI <- dashboardPage(header, sidebar, body)