install.packages("shiny")
install.packages("memoise")
install.packages("DBI")
install.packages("pool")
install.packages("RMySQL")
devtools::install_github("rstudio/pool")
library("RMySQL")
library(pool)
library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(DBI)
library(dplyr)
library(stringr)
library(tokenizers)
library(tidytext)
library(magrittr)
library(tidyr)
library(wordcloud)
library(reshape2)
library(igraph)
library(ggraph)
library(widyr)
library(DT)
mydb <- dbConnect(RSQLite :: SQLite(), dbname = "fake.db")
#table1 <- dbGetQuery(mydb,"SELECT text,segment.user_id,parsed_created_at,group_name FROM fake_account JOIN segment ON segment.user_id = fake_account.user_id WHERE parsed_created_at BETWEEN '2011-03-01' AND '2011-05-01'")
#table2 <- dbGetQuery(mydb,"SELECT text,segment.user_id,parsed_created_at,group_name,cluster_name FROM fake_account JOIN segment ON segment.user_id = fake_account.user_id WHERE parsed_created_at BETWEEN '2017-01-01' AND '2018-01-01'")
#table3 <- dbGetQuery(mydb,"SELECT text,segment.user_id,parsed_created_at,group_name,cluster_name FROM fake_account JOIN segment ON segment.user_id = fake_account.user_id WHERE parsed_created_at BETWEEN '2016-10-01' AND '2016-11-01'")
#dbWriteTable(mydb,"temp",table1)
#dbWriteTable(mydb,"temp3",table3)
#dbWriteTable(mydb,"temp2",table2,overwrite=T)
#dbListTables(mydb)
#dbListFields(mydb, 'temp3')
pool <- dbPool(
  drv = RSQLite :: SQLite(),
  dbname = "fake.db"
)
group <<- list("Trump Support" = "Trump Support",
               "Conservative" = "Conservative",
               "Hard Conservative"="Hard Conservative",
               "Intl Right|Anti-Islam"="Intl Right|Anti-Islam",
               "Other"="Other",
               "Russia" = "Russia")
#cluster<<-list()
color<-c("grey80", "darkgoldenrod1","red")
data(stop_words)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
SEED = 2018;
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}
#ui
ui <- fluidPage(
  #titlePanel("Word Cloud"),
  tabsetPanel(
    tabPanel(
  # Application title
  "Word Cloud",fluid=TRUE,
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("group", "Choose a group:",
                  choices = group),
      #actionButton("update", "Change"),
      # Pass in a Date object
      #dateInput("date4", "Date:", value = Sys.Date()-10),
      # Start with decade view instead of default month view
      #title
      dateInput("date1", "From Date:",value='2016-10-01'),
      dateInput("date2", "To Date:",value='2016-11-01'),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 2000, value = 100),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  #tabPanel("textoutput",verbatimTextOutput("Summary")),
                  tabPanel("Word Cloud", plotOutput("plot")),
                  tabPanel("Sentiment Analysis", plotOutput("plot3"),plotOutput("plot2")),
                  tabPanel("Bigram", plotOutput("plot4")))
  ))),
  tabPanel("Topic Modeling",fluid=TRUE,
  sidebarLayout(
  sidebarPanel(
      #actionButton("update", "Change"),
      dateInput("date3", "From Date:",value='2016-10-01',startview = "decade"),
      dateInput("date4", "To Date:",value='2016-11-01',startview = "decade"),
      hr(),
      numericInput("topic", label = h3("Number of topics"), value =2,max=10),
      numericInput("words", label = h3("Top n words in each topic"), value =10,max = 30),
      downloadButton('downloadData', 'Download')
    ),
    # Show Word Cloud
    mainPanel(
      tabsetPanel(type = "tabs",
                  #tabPanel("textoutput",verbatimTextOutput("Summary")),
                  tabPanel("Top Words in Each Topic",dataTableOutput('table'))
                  #tabPanel("Sentiment Analysis", plotOutput("plot3"),plotOutput("plot2")),
                  #tabPanel("Bigram", plotOutput("plot4")))
      )
    )
)
)))
server <- function(input, output, session) {
  output$plot <- renderPlot({
    sql <- "SELECT text FROM temp3 WHERE parsed_created_at BETWEEN ?date1 AND ?date2 and group_name =?group;"
    query <- sqlInterpolate(pool, sql, group = input$group,date1 = format(input$date1,format = "%Y-%m-%d"),date2=format(input$date2,format="%Y-%m-%d"))
    df <- dbGetQuery(pool, query)
    #table1 <- dbGetQuery(mydb,"SELECT text,segment.user_id,parsed_created_at,group_name FROM fake_account JOIN segment ON segment.user_id = fake_account.user_id WHERE parsed_created_at BETWEEN '2011-03-01' AND '2011-03-03' and group_name = input$group;")
    text <- df$text
    text <- as.character(text)
    text <- iconv(text, 'utf-8', 'ascii', sub='')
    clean_tweet = gsub("&amp", "", text)
    clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
    clean_tweet = gsub("@\\w+", "", clean_tweet)
    clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
    clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
    clean_tweet = gsub("http\\w+", "", clean_tweet)
    clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
    clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet)
    tweet_source <- VectorSource(clean_tweet)
    tweet_corpus <- VCorpus(tweet_source)
    tweet_corpus <- clean_corpus(tweet_corpus)
    myTDM = TermDocumentMatrix(tweet_corpus,control = list(minWordLength = 1))
    m = as.matrix(myTDM)
    term_frequency<-rowSums(m)
    term_frequency<-sort(term_frequency,decreasing=TRUE)
    word_freqs<-data.frame(term=names(term_frequency),num=term_frequency)
    wordcloud(word_freqs$term, 
              word_freqs$num, 
              min.freq = input$freq,
              max.words = input$max,
              #scale=c(4,0.5),
              colors = brewer.pal(8, "Dark2"))
  })
    output$plot2 <- renderPlot({
      sql <- "SELECT text,user_id FROM temp3 WHERE parsed_created_at BETWEEN ?date1 AND ?date2 and group_name =?group;"
      query  <- sqlInterpolate(pool, sql, group = input$group,date1 = format(input$date1,format = "%Y-%m-%d"),date2=format(input$date2,format="%Y-%m-%d"))
      df <- dbGetQuery(pool, query)
      tweet <- tbl_df(df)
      text2 <- as.character(tweet$text)
      text2 <- iconv(text2, 'utf-8', 'ascii', sub='')
      clean_tweet2 = gsub("&amp", "", text2)
      clean_tweet2 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet2)
      clean_tweet2 = gsub("@\\w+", "", clean_tweet2)
      clean_tweet2 = gsub("[[:punct:]]", "", clean_tweet2)
      clean_tweet2 = gsub("[[:digit:]]", "", clean_tweet2)
      clean_tweet2 = gsub("http\\w+", "", clean_tweet2)
      clean_tweet2 = gsub("[ \t]{2,}", "", clean_tweet2)
      clean_tweet2 = gsub("^\\s+|\\s+$", "", clean_tweet2)
      tweet$text <- clean_tweet2
      tidy_tweet <- tweet %>%
        unnest_tokens(word, text)
      tidy_tweet <- tidy_tweet %>%
        anti_join(stop_words)%>%
        filter(word != "trump")
      tidy_tweet %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("gray20", "tomato"),
                         max.words = input$max)
    })
    output$plot3 <- renderPlot({
      sql <- "SELECT text,user_id FROM temp3 WHERE parsed_created_at BETWEEN ?date1 AND ?date2 and group_name =?group;"
      query  <- sqlInterpolate(pool, sql, group = input$group,date1 = format(input$date1,format = "%Y-%m-%d"),date2=format(input$date2,format="%Y-%m-%d"))
      df <- dbGetQuery(pool, query)
      tweet <- tbl_df(df)
      text2 <- as.character(tweet$text)
      text2 <- iconv(text2, 'utf-8', 'ascii', sub='')
      clean_tweet2 = gsub("&amp", "", text2)
      clean_tweet2 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet2)
      clean_tweet2 = gsub("@\\w+", "", clean_tweet2)
      clean_tweet2 = gsub("[[:punct:]]", "", clean_tweet2)
      clean_tweet2 = gsub("[[:digit:]]", "", clean_tweet2)
      clean_tweet2 = gsub("http\\w+", "", clean_tweet2)
      clean_tweet2 = gsub("[ \t]{2,}", "", clean_tweet2)
      clean_tweet2 = gsub("^\\s+|\\s+$", "", clean_tweet2)
      tweet$text <- clean_tweet2
      tidy_tweet <- tweet %>%
        unnest_tokens(word, text)
      tidy_tweet <- tidy_tweet %>%
        anti_join(stop_words)%>%
        filter(word != "trump")
      bing_word_counts <- tidy_tweet %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()
      bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(20) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()
    })
    output$plot4 <- renderPlot({
      sql <- "SELECT text,user_id FROM temp3 WHERE parsed_created_at BETWEEN ?date1 AND ?date2 and group_name =?group;"
      query  <- sqlInterpolate(pool, sql, group = input$group,date1 = format(input$date1,format = "%Y-%m-%d"),date2=format(input$date2,format="%Y-%m-%d"))
      df <- dbGetQuery(pool, query)
      tweet3 <- tbl_df(df)
      text3 <- as.character(tweet3$text)
      text3 <- iconv(text3, 'utf-8', 'ascii', sub='')
      clean_tweet3 = gsub("&amp", "", text3)
      clean_tweet3 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet3)
      clean_tweet3 = gsub("@\\w+", "", clean_tweet3)
      clean_tweet3 = gsub("[[:punct:]]", "", clean_tweet3)
      clean_tweet3 = gsub("[[:digit:]]", "", clean_tweet3)
      clean_tweet3 = gsub("http\\w+", "", clean_tweet3)
      clean_tweet3= gsub("[ \t]{2,}", "", clean_tweet3)
      clean_tweet3 = gsub("^\\s+|\\s+$", "", clean_tweet3)
      tweet3$text <- clean_tweet3
      tweet_bigrams <- tweet3 %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        na.omit()
      bigrams_separated <- tweet_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")
      bigrams_filtered <- bigrams_separated %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word)
      # new bigram counts:
      bigram_counts <- bigrams_filtered %>% 
        count(word1, word2, sort = TRUE)
      #bigrams_separated %>%
        #filter(word2 == "makeamericagreatagain") %>%
        #count(word1, sort = TRUE)
      bigram_graph <- bigram_counts %>%
        filter(n > 10) %>%
        graph_from_data_frame()
      set.seed(2018)
      ggraph(bigram_graph, layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                       arrow = a, end_cap = circle(.07, 'inches')) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()
      #word_cors %>%
      #filter(item2 == "gun")
    })
    output$table <- renderDataTable({
      sql <- "SELECT text,user_id FROM temp3 WHERE parsed_created_at BETWEEN ?date3 AND ?date4;"
      query <- sqlInterpolate(pool, sql,date3 = format(input$date3,format = "%Y-%m-%d"),date4=format(input$date4,format="%Y-%m-%d"))
      df <- dbGetQuery(pool, query)
      tweet5 <- tbl_df(df)
      text5 <- as.character(tweet5$text)
      text5 <- iconv(text5, 'utf-8', 'ascii', sub='')
      clean_tweet5 = gsub("&amp", "", text5)
      clean_tweet5 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet5)
      clean_tweet5 = gsub("@\\w+", "", clean_tweet5)
      clean_tweet5 = gsub("[[:punct:]]", "", clean_tweet5)
      clean_tweet5 = gsub("[[:digit:]]", "", clean_tweet5)
      clean_tweet5 = gsub("http\\w+", "", clean_tweet5)
      clean_tweet5 = gsub("[ \t]{2,}", "", clean_tweet5)
      clean_tweet5 = gsub("^\\s+|\\s+$", "", clean_tweet5)
      tweet5$text <- clean_tweet5
      tidy_tweet5 <- tweet5 %>%
        unnest_tokens(word, text)
      tidy_tweet5 <- tidy_tweet5 %>%
        anti_join(stop_words)
      tidy_tweet_id <- tidy_tweet5 %>%
        count(user_id, word)
      #create dtm according to user id
      new_dtm <- tidy_tweet_id %>%
        cast_dtm(user_id, word, n)
      rowTotals <- apply(new_dtm , 1, sum) #Find the sum of words in each Document
      new_dtm <- new_dtm[rowTotals> 0, ]           #remove all docs without words
      #LDA with Gibbs Sampling
      tweet.lda <- LDA(new_dtm, input$topic, method="Gibbs", control=list(seed = SEED))
      #print the 10 most important words in the topic
      lda.terms <- as.table(terms(tweet.lda,input$words))
      datatable(lda.terms)
    })  
    datasetInput <- reactive(lda.terms)
    output$downloadData <- downloadHandler(
      filename = function() {"topwords.csv" },
      content = function(file) {
        write.csv(datasetInput(), file)
        })
}
shinyApp(ui = ui, server = server)

