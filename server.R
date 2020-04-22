#Load required libraries
library(textreadr)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(igraph)
library(ggraph)
library(reshape2)
library(topicmodels)
library(quanteda)
library(tm)
library(ROCR)
library(naivebayes)
library(e1071)
library(klaR)
library(caret)
library(gmodels)
library(knitr)
library(coefplot)
library(readxl)

#function for tokenizing
tokenizing <- function (question, custom=NULL){
    if(is.null(custom)) {custom <- head(stop_words,1)} #if custom is null select first record from stop_words
    # Tokenizing question
    frequency <- question %>%
        unnest_tokens(word, text) %>%   #tokenizing
        anti_join(stop_words) %>%       #remove common tokens
        anti_join(custom) %>%           #remove custom tokens
        count(word, sort=TRUE) %>%      #count frequency of words
        ungroup()
    #return dataframe with frequencies
    return (frequency)
}

#Function for getting bigrams
bi_grams <- function (data, custom=NULL) {
    if(is.null(custom)) {custom <- head(stop_words,1)} #if custom is null select first record from stop_words
    tokens <- data %>%
        unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        filter(!word1 %in% custom$word) %>%
        filter(!word2 %in% custom$word) %>%
        count(word1, word2, sort = TRUE) %>%
        arrange(-n)
    return (tokens)
}

#function to plot frequencies
plot_freq <- function (data, top=NULL, color='navy') {
    if (is.null(top)){top<-nrow(frequencies)} #evaluate arg top
freq <- data %>%
        unnest_tokens(word, text) %>%   #tokenizing
        anti_join(stop_words) %>%       #remove common tokens
        #anti_join(custom) %>%          #remove custom tokens
        count(word, sort=TRUE) %>%      #count frequency of words
        ungroup()    
plot <- freq %>%  
        arrange(desc(n),word) %>%
        head(top) %>%
        ggplot(aes(reorder(word,-n), n))+
        geom_bar(stat='identity', fill=color)+
        xlab(NULL)+
        coord_flip()+
        theme_light()
return(plot)
}

#function to analyze sentiments
sentiments_lex <- function (survey) {
    #tokenizing
    question <- survey %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        count(word, sort=T) %>%
        ungroup()
    
    nrc   <- get_sentiments ("nrc") %>% filter(sentiment %in% c("positive", "negative"))
    bing  <- get_sentiments ("bing")
    afinn <- get_sentiments ("afinn")
    
    #create a index
    question <- question %>%
        mutate(linenumber = row_number())
    
    #create a dataframe with affin lexicon
    q_affin <- question %>%
        inner_join(afinn) %>%
        group_by(index=linenumber) %>%
        summarise(sentiment=sum(value)) %>%
        mutate(method = "AFINN")
    
    #create a dataframe with nrc and bing lexicon
    q_nrcbing <- bind_rows (
        question %>%
            inner_join(nrc) %>% 
            mutate(method = "NRC"),
        question %>%
            inner_join(bing) %>% 
            mutate(method = "BING")) %>%
        count(method, index=linenumber, sentiment) %>%
        spread(sentiment, n, fill=0) %>%
        mutate(sentiment = positive-negative)
    #Plot sentiments based in each lexicon
plot <- bind_rows (q_nrcbing, q_affin) %>%
        ggplot(aes(index, sentiment, fill=method))+
        geom_col(show.legend=FALSE)+
        facet_wrap(~method, ncol =1, scales= "free_y")
return(plot)
}

#function to analyze sentiments (positives and negatives)
sentiments <- function (survey, top=NULL) {
    if (is.null(top)) {top=nrow(survey)}
    sentiment <- survey %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort=T) %>%
        ungroup()
    
    plot <- sentiment %>%
        group_by(sentiment) %>%
        top_n(top) %>%
        ungroup() %>%
        mutate(word=reorder(word, n)) %>%
        ggplot(aes(word, n, fill=sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y")+
        labs(y="Contribution to sentiment", x=NULL)+
        coord_flip()
return(plot)
}  

#function to analyze sentiments
wordcloud <- function (survey, top=NULL) {
    if (is.null(top)) {top=nrow(survey)} 
    plot <- survey %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort=T) %>%
        acast(word ~sentiment, value.var="n", fill=0) %>%
        comparison.cloud(colors = c("red", "Navy"),
                         max.words=100,
                         title.size=2,
                         title.colors = c("red", "Navy"))
return(plot)
}  

#Function for TF-IDF analysis
tf_idf <- function(survey, top=NULL) {
    if (is_null(top)) {top=nrow(survey)}
    plot<-  survey %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        count(question, word, sort = TRUE) %>%
        bind_tf_idf(word, question, n) %>%
        group_by(question) %>%
        mutate(rank = row_number()) %>%
        filter (rank <= top) %>%
        ggplot(aes(reorder(word, tf_idf) ,tf_idf, fill=question))+
        geom_col(show.legend=FALSE)+
        labs(x=NULL, y="tf-idf", title='TF-IDF Analysis')+
        facet_wrap(~question, ncol=2, scales="free")+
        coord_flip()
return(plot)
}

#function to plot a word network
network <- function(data, lower=1) {
    set.seed(1234)
    data_graph <- data %>%
        filter(n >= lower) %>%
        graph_from_data_frame()
    #Show words network
    plot <- ggraph(data_graph, layout = "fr") +  
        geom_edge_link()+
        geom_node_point(size = 3, shape = 21, stroke = 1,
                        fill = 'maroon3', color = 'black') +
        geom_node_text(aes(label=name), vjust=1.5, hjust=1.5) +
        theme(legend.position="none")
return(plot)
}

#topic beta analysis
top_beta <- function (survey_dtm, top=10) { 
    survey_lda <- LDA(survey_dtm,k=2, control = list(seed=123)) #Get topics    
    survey_topics <- tidy(survey_lda, matrix="beta") # Running LDA per token - beta matrix
    
    # Looking at the probabilities of the top terms
plot <- survey_topics %>%
        group_by(topic) %>%
        top_n(top, beta) %>%
        ungroup() %>%
        # Lets plot the term frequencies by topic
        mutate(term=reorder(term, beta)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend=FALSE) +
        facet_wrap(~topic, scales = "free") +
        coord_flip()
return(plot)
}

#topic gamma analysis
top_gamma <- function (survey_dtm) {
    survey_lda <- LDA(survey_dtm,k=2, control = list(seed=123)) #Get topics
plot <- tidy(survey_lda, matrix="gamma") %>%
        arrange(document, desc(gamma)) %>%
        ggplot(aes(document, gamma, fill = factor(topic))) +
        geom_col(show.legend=FALSE) +
        facet_wrap(~topic, scales = "free") +
        labs (x="questions") +
        coord_flip()
return(plot)
}

get_df <- function(survey, number="1") {
    if (number != "7") {
        survey <- survey %>%
        filter (str_detect(question, number))
    }
    print(c("rows:",nrow(survey)))
    return(survey)
}


predictive <- function(my_df, size){
    my_df$binary <- c(0,0,1,1,0,1,
                      0,0,0,1,0,1,
                      1,0,1,1,0,1,
                      1,0,0,1,1,0,
                      1,1,0,1,0,0,
                      1,1,1,1,1,1,
                      1,1,1,1,0,0,
                      0,0,1,0,1,1,
                      1,1,0,0,0,1,
                      0,1,1,0,1,1,
                      0,1,0,1,0,1)   
    
    #let's split the respondent into training and testing data
    my_df$binary <- as.factor((my_df$binary))
    
    my_df_pred_train <- sample(nrow(my_df), ceiling(nrow(my_df) * size))
    my_df_pred_test <- my_df[-my_df_pred_train,]
    my_df_pred_train <- my_df[my_df_pred_train,]
    
    #building the Naive Bayes model:
    NBclassfier <- naiveBayes(binary~Question1+Question2+Question3+Question4+Question5+Question6, data=my_df_pred_train)
    
    
    trainPred <- predict(NBclassfier, newdata = my_df_pred_train, type = "class")
    trainTable <- table(my_df_pred_train$binary, trainPred)
    testPred <- predict(NBclassfier, newdata=my_df_pred_test, type="class")
    testTable <- table(my_df_pred_test$binary, testPred)
    trainAcc <- (trainTable[1,1]+trainTable[2,2])/sum(trainTable)
    testAcc <- (testTable[1,1]+testTable[2,2])/sum(testTable)
    
    accuracy <- round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3)
    
    pred <- predict(NBclassfier, newdata=my_df, type = "raw")
    
    #Plot ROC Curve
    pred_log <- prediction(pred[,2], my_df$binary) 
    perf <- performance(pred_log,"tpr","fpr")
    plot(perf,col="purple",lty=3, lwd=3)
    
}

accu <- function(my_df, size){
    my_df$binary <- c(0,0,1,1,0,1,
                      0,0,0,1,0,1,
                      1,0,1,1,0,1,
                      1,0,0,1,1,0,
                      1,1,0,1,0,0,
                      1,1,1,1,1,1,
                      1,1,1,1,0,0,
                      0,0,1,0,1,1,
                      1,1,0,0,0,1,
                      0,1,1,0,1,1,
                      0,1,0,1,0,1)   
    
    #let's split the respondent into training and testing data
    my_df$binary <- as.factor((my_df$binary))
    
    my_df_pred_train <- sample(nrow(my_df), ceiling(nrow(my_df) * size))
    my_df_pred_test <- my_df[-my_df_pred_train,]
    my_df_pred_train <- my_df[my_df_pred_train,]
    
    #building the Naive Bayes model:
    NBclassfier <- naiveBayes(binary~Question1+Question2+Question3+Question4+Question5+Question6, data=my_df_pred_train)
    
    
    trainPred <- predict(NBclassfier, newdata = my_df_pred_train, type = "class")
    trainTable <- table(my_df_pred_train$binary, trainPred)
    testPred <- predict(NBclassfier, newdata=my_df_pred_test, type="class")
    testTable <- table(my_df_pred_test$binary, testPred)
    trainAcc <- (trainTable[1,1]+trainTable[2,2])/sum(trainTable)
    testAcc <- (testTable[1,1]+testTable[2,2])/sum(testTable)
    
    accu <- round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3)
    print(accu)
    return(accu[,2])
}

words_analysis <- function (my_df) {
    survey1 <- data_frame(my_df)
    survey1$text <- paste(my_df$Question1, my_df$Question2, my_df$Question3, my_df$Question4, my_df$Question5, my_df$Question6)
    #Define success observations
    
    trainingset <- dfm(survey1$text, tolower = FALSE)
    survey1$binary <- c(0,0,1,1,0,1,
                        0,0,0,1,0,1,
                        1,0,1,1,0,1,
                        1,0,0,1,1,0,
                        1,1,0,1,0,0,
                        1,1,1,1,1,1,
                        1,1,1,1,0,0,
                        0,0,1,0,1,1,
                        1,1,0,0,0,1,
                        0,1,1,0,1,1,
                        0,1,0,1,0,1)   
    survey1$binary[41:66] <- NA
    trainingclass <- factor(survey1$binary, ordered = TRUE)
    
    ## replicate IIR p261 prediction for test set (document 5)
    tmod1 <- textmodel_nb(trainingset, y = trainingclass, prior = "docfreq")
    print(tmod1)
    return(my_df)
}

fprediction <- function(my_df, X.selection, X.ctry, X.activity, X.gender, X.age, X.fav){
    
    p <- data.frame(X.selection, X.ctry, X.activity, X.gender, X.age, X.fav)
    
    my_df$text <- paste(my_df$V1, my_df$V2, my_df$V3, my_df$V4, my_df$V5, my_df$V6)
    p$text <- paste(p$X.selection, p$X.ctry, p$X.activity, p$X.gender, p$X.age, p$X.fav)
    
    p$binary <- 0
    my_df$binary <- c(0,0,1,1,0,1,
                      0,0,0,1,0,1,
                      1,0,1,1,0,1,
                      1,0,0,1,1,0,
                      1,1,0,1,0,0,
                      1,1,1,1,1,1,
                      1,1,1,1,0,0,
                      0,0,1,0,1,1,
                      1,1,0,0,0,1,
                      0,1,1,0,1,1,
                      0,1,0,1,0,1)   
    
    #create dataframe to be used for predictive model
    my_df_pred <- data_frame(text=my_df$text, binary = my_df$binary)
    test_pred <- data_frame(text=p$text, binary = p$binary)
    
    #create a regular corpus structure
    my_df_corpus <- corpus(my_df_pred$text) #creating the corpus on the $text var
    msg.dfm <- dfm(my_df_corpus, tolower = TRUE) #generating document 
    msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 3, min_docfreq = 0)
    
    test_corpus <- corpus(test_pred$text) #creating the corpus on the $text var
    dfm.test <- dfm(test_corpus, tolower = TRUE) #generating document 
    #dfm.test <- dfm_trim(dfm.test, min_termfreq = 3, min_docfreq = 0)
    #msg.dfm <- dfm_weight(msg.dfm, type = "tfidf")
    
    #building the Naive Bayes model:
    NB_classifier <- textmodel_nb(msg.dfm, my_df$binary) # we need to tell which 1 and 0 to use
    
    
    return(predict(NB_classifier, dfm.test, force = TRUE))
}

############################################################
# Initialization                                           #
############################################################

# Loading the .txt file into R
#survey_ans <- read_document(file="/Users/deniz/Desktop/HULT_Stuff/DUAL DEGREE MBAN/SPRING/TEXT ANALYSIS/group/NLP Survey Answers.txt")
survey_ans <- read_document(file="/Users/linhle/Desktop/Text Analytics/NLP Survey | Team Project/NLP Survey Answers.txt")
words  <- as.data.frame(read_excel("/Users/linhle/Desktop/Text Analytics/NLP Survey | Team Project/SHINY/token_probability.xlsx", col_names = T))

#Define parameters and create a empty dataframe
rows <- 66 #how many observations to you have - how many people you have
cols <- 6 #how many variables do you have - how many answers per person
my_df <- as.data.frame(matrix(nrow=rows, ncol=cols))

# Creating a nested for loop to fill in dataframe with corresponding line item
for(z in 1:cols){
    for(i in 1:rows){
        my_df[i,z]<- survey_ans[i*cols+z-cols]
    }#closing z loop
}#closing i loop

#Custom stop_words
#custom <- data_frame(word=c('based', 'depends', 'depend', 'depending'), lexicon=c('cust', 'cust', 'cust', 'cust'))
#custom <- data_frame(word=c('shoes', 'shoe'), lexicon=c('cust', 'cust'))

#Create a dataframe for each question
q1 <- data_frame(text=my_df$V1)
q2 <- data_frame(text=my_df$V2)
q3 <- data_frame(text=my_df$V3)
q4 <- data_frame(text=my_df$V4)
q5 <- data_frame(text=my_df$V5)
q6 <- data_frame(text=my_df$V6)
questions <- c("Question1", "Question2", "Question3", 
               "Question4", "Question5", "Question6")
colnames(my_df) <- questions
survey <- bind_rows(mutate(q1, question = questions[1]),
                    mutate(q2, question = questions[2]),
                    mutate(q3, question = questions[3]),
                    mutate(q4, question = questions[4]),
                    mutate(q5, question = questions[5]),
                    mutate(q6, question = questions[6]))

#DTM Survey
survey_dtm <- survey %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(question, word, sort=TRUE) %>%
    cast_dtm(question, word, n)      

#######################################
server <- function(input, output) {
    df <- reactive({ get_df(survey, substr(input$question,1,1)) })
    number <- reactive({substr(input$question,1,1)})
   
    output$gender <-  renderValueBox({
        valueBox(formatC("31M 29F 6T", format="c", big.mark=',')
                 ,'Gender'
                 ,icon = icon("venus-mars",lib='glyphicon')
                 ,color = "light-blue"
        ) }) 
    
    output$country <-  renderValueBox({
        valueBox(formatC(23, format="d", big.mark=',')
                 ,'Countries'
                 ,icon = icon("globe",lib='glyphicon')
                 ,color = "light-blue"
        ) })
    
    output$age <-  renderValueBox({
        valueBox(formatC("20-74", format="c", big.mark=',')
                 ,'Age Range'
                 ,icon = icon("hashtag",lib='glyphicon')
                 ,color = "light-blue"
        ) })
    
    output$problem <-  renderInfoBox({
        infoBox(title=NULL, 
                value = tags$p("Sales for Sneaker in San Francisco are declining. 
                          Foot Locker wants to know the public opinion and purchasing behaviour to optimize marketing campaigns.", style = "font-size: 150%;"), 
                subtitle = NULL,
                icon = shiny::icon("bolt"), color = "aqua", 
                href = NULL, fill = FALSE)
    })
    
    # output$pred <-  renderInfoBox({
    #     infoBox(title=NULL, 
    #             value = tags$p("XXXX XXXX", style = "font-size: 150%;"), 
    #             subtitle = "Buyers",
    #             icon = shiny::icon("dollar-sign"), color = "aqua", 
    #             href = NULL, fill = FALSE)
    # })

    output$senti <-  renderInfoBox({
        infoBox(title=NULL, 
                value = tags$p(" +", style = "font-size: 300%;"), 
                subtitle = "Overall Sentiment",
                icon = shiny::icon("smile"), color = "aqua", 
                href = NULL, fill = FALSE)
    })
    output$pred <-  renderInfoBox({
        infoBox(title=NULL, 
                value = tags$p("69%", style = "font-size: 300%;"), 
                subtitle = "Accuracy of Model",
                icon = shiny::icon("smile"), color = "aqua", 
                href = NULL, fill = FALSE)
    })
    output$pers <-  renderInfoBox({
        infoBox(title=NULL, 
                value = tags$p("Persona", style = "font-size: 200%;"), 
                subtitle = c("22 - 29, Comfort, Function, Team"),
                icon = shiny::icon("child"), color = "aqua", 
                href = NULL, fill = FALSE)
    })
    
    observeEvent(input$action, {
        output$prediction <- renderInfoBox({
            infoBox(title=NULL,
    
                    value = tags$p(fprediction(my_df, input$selection, input$ctry, input$activity, input$gender, input$age, input$fav), style = "font-size: 280%;"), 
                    subtitle = "Prediction",
                    icon = shiny::icon("smile"), color = "aqua", 
                    href = NULL, fill = FALSE)
        })
    })
    
#Insights    
    # output$accu <-  renderValueBox({
    #     valueBox(as.String(accu(my_df,input$slide/100)), "Accurracy"
    #              
    #     ) })
    output$accu <-  renderInfoBox({
        infoBox(title="Accuracy of Model",
                value = tags$p(accu(my_df,input$slide/100)*100, style = "font-size: 150%;"),
                subtitle = NULL,
                icon = shiny::icon("percent"), color = "aqua",
                href = NULL, fill = FALSE)
    })

    output$Bin2 <-  renderValueBox({
        valueBox(formatC("Comfort", format="c", big.mark=',')
                 ,'Success'
                 ,icon = icon("venus-mars",lib='glyphicon')
                 ,color = "light-blue"
        ) }) 
    
    output$Bin3 <-  renderValueBox({
        valueBox(formatC("Run/Sports", format="c", big.mark=',')
                 ,'Success'
                 ,icon = icon("walk",lib='glyphicon')
                 ,color = "light-blue"
        ) })

    
    output$Bin4 <-  renderValueBox({
        valueBox(formatC("25", format="c", big.mark=',')
                 ,'Age'
                 ,icon = icon("hashtag",lib='glyphicon')
                 ,color = "light-blue"
        ) })
    
    output$in1 <-  renderInfoBox({
        infoBox(title= " Recommendations", 
                value = tags$p("
Engage in targeted ad marketing campaigns towards the Millennials and Generation Z (aged 22-29) who have active lifestyles", style = "font-size: 150%;"), 
                subtitle = NULL,
                icon = shiny::icon("bolt"), color = "aqua", 
                href = NULL, fill = FALSE)
    })
    
    output$in2 <-  renderInfoBox({
        infoBox(title=NULL, 
                value = tags$p("Target gyms and billboards in areas that have high rates of outdoor activities", style = "font-size: 150%;"), 
                subtitle = NULL,
                icon = shiny::icon("dumbbell"), color = "aqua", 
                href = NULL, fill = FALSE)
    })
    
    output$in3 <-  renderInfoBox({
        infoBox(title="Success Tokens", 
                value = tags$p("25, Sports, Run", style = "font-size: 150%;"), 
                subtitle = NULL,
                icon = shiny::icon("check-circle"), color = "aqua", 
                href = NULL, fill = FALSE)
    })
    
    output$in4 <-  renderInfoBox({
        infoBox(title="Failure Tokens", 
                value = tags$p("Loafers, Crocs, 56", style = "font-size: 150%;"), 
                subtitle = NULL,
                icon = shiny::icon("times-circle"), color = "aqua", 
                href = NULL, fill = FALSE)
    })
    
    # output$pers <-  renderInfoBox({
    #     infoBox(title=NULL, 
    #             value = tags$p("Persona", style = "font-size: 150%;"), 
    #             subtitle = c("22 - 29     
    #                         Comfort
    #                         Function
    #                         Team"),
    #             icon = shiny::icon("child"), color = "aqua", 
    #             href = NULL, fill = FALSE)
    # })
    
    output$accuracy <- renderValueBox({
            valueBox(formatC(70, format="d", big.mark=',')
                    ,'accuracy'
                   ,icon = icon("percentage",lib='glyphicon')
                  ,color = "light-blue"
            )
    })
   
    
    
    #plot word frequencies
    output$frequencies <- renderPlot({plot_freq(df(),top=10,color='red') })
    #Plot sentiments by lexicon
    output$lexicon <- renderPlot({ sentiments_lex(survey) })
    #plot positive and negative sentiment
    output$sentiment <- renderPlot({ sentiments(df()) })
    #plot word network 
    output$network <-   renderPlot({ network(bi_grams(df())) })
    #plot word cloud
    output$wordcloud <- renderPlot({ wordcloud(df()) })
    #plot word cloud
    output$tfidf <- renderPlot({ tf_idf(survey, 10) })
    #plot beta analysis
    observeEvent(input$beta, {
        output$topic <- renderPlot({ top_beta(survey_dtm) })
    })
    
    observeEvent(input$gamma, {
        output$topic <- renderPlot({ top_gamma(survey_dtm) })
    }
    )
    #output$beta <- renderPlot({ top_beta(survey_dtm) })
    #plot gamma analysis
    #output$gamma <- renderPlot({ top_gamma(survey_dtm) })

    #plot accuracy 
    output$predictive <- renderPlot({ predictive(my_df, input$slide/100) })
    output$words <- renderTable(words)
    
}


