#### Bunch of libraries ####

library(MAP)
library(forcats)
library(plyr)

library(ggplot2)
library(ggmosaic)
library(esquisse)
library(shiny)
library(shinydashboard)

library(readxl)
library(janitor)
library(magrittr)

library(maps)
library(maptools)
library(mapproj)
library(ggmap)

library(leaflet)
library(leaflet.extras)

library(wordcloud)

library(gutenbergr)
library(tidyverse)
library(tidytext)

#### Books to Focus On ####
booksId = sort(c( 1232, 1129, 1122, 1120, 1118, 1112, 1004, 996, 981, 
                  832, 714, 514, 345, 341, 236, 228, 215, 203, 
                  175, 174, 164, 161, 150, 147, 141, 140, 135, 113, 
                  108, 103, 99, 98, 84, 77, 73, 61, 57, 46, 45, 
                  41, 35, 33, 30, 20, 16, 15))

numBooks = length(booksId)

miniMeta <- gutenberg_metadata %>% filter(gutenberg_id %in% booksId )

bookTitles <-as.vector(miniMeta$title)

bookData <- as.data.frame(cbind(booksId, bookTitles)) %>%  arrange(bookTitles)

bookTitles <- sort(bookTitles)

#### Country Cleaning ####

data("world.cities") #data set with lists of cities and countries 

#set up the world data set to use for the latitudes and longitudes 
world <- world.cities %>% 
  rename(country = country.etc) %>% 
  filter(capital == 1)%>% select(-c(pop,capital))

world$country <- tolower(world$country)
world$country[world$country == "korea north"] <- "north korea"
world$country[world$country == "korea south"] <- "south korea"
world$country[world$country == "uk"] <- "united kingdom"
world$country[world$country == "usa"] <- "united states"

newGB <- c("london", "great britain", 	51.52, 	-0.10, 1)
newEngland <- c("london", "england", 	51.52, 	-0.10, 1)
newUS <- c("washington", "united states of america", 38.91, -77.02, 1)
newScotland <- c("edinburgh", "scotland", 55.95, -3.22, 0)
world <- rbind(world, newGB, newEngland, newUS,newScotland)

#list of countries 
countries <- unique(world$country)
#character, vector, not a data frame  

#gets the sentiment 
snrc <- get_sentiments("nrc")




#### SHINY User Interface ####
ui2 <- dashboardPage(
  dashboardHeader(title = "Literary Comparison Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      radioButtons("option", "What Do You Want to Explore?",
                   c("My Sentiments Exactly" ="senti",
                     "Word Clouds" = "cloud",
                     "Maps of Country References" = "maps")),
      selectInput("book1", "First, take a look in this book", bookTitles),
      selectInput("book2", "Now, take a look in this book", bookTitles)
      #sliderInput("numWords ", "Number of Words to Analyze",  
                  #min = 10, max = 100, value = 1)
      
    )#ENDS sidebarMenu

      
  ),#ENDS dashboardSidebar
  
  
  dashboardBody(
      
      fluidRow(
        splitLayout(style = "border: 1px solid silver:", cellWidths = c(350,350), 
                    plotOutput("senti1"),
                    plotOutput("senti2")
        )#ENDS splitLayout
      ),
     
      
      fluidRow(
        splitLayout(style = "border: 1px solid silver:", cellWidths = c(350,350), 
                           plotOutput("cloud1"),
                           plotOutput("cloud2")
      )#ENDS splitLayout
      ),#ENDS fluidRow
      fluidRow(
        splitLayout(style = "border: 1px solid silver:", cellWidths = c(350,350), 
                    tableOutput("freq1"),
                    tableOutput("freq2"))#ENDS splitLayout
      ),#ENDS fluidRow
      leafletOutput("map1"),
      leafletOutput("map2")
      
    #)# Ends Fluid Row 
             
             
             
             
    #ENDS mainPanel
    
  )#ENDS dashboardBody
)

#Noteability 

#### SHINY Server ####
server2 <- function(input, output) {

  
  ######### Sets up word frequencies for the two books - WORKS #########
  wordFreqsOfChosen1 <- reactive({
    bookName = input$book1 # name of book chosen 
    gutNum <- bookData[which(bookData$bookTitles == bookName),1] #gutenberg id chosen book 
    chosenBook <- gutenberg_download(gutNum) #gets book data 
    chosenBook %<>%
      unnest_tokens(word, text) %>% #isolates each word into its own row 
      anti_join(stop_words) # gets rid of the stop words 
    bkCount <-  chosenBook %>% count(word, sort = TRUE) #gets a word count 
    return(bkCount) #this is a dataframe 
  })
  
  wordFreqsOfChosen2 <- reactive({
    bookName = input$book2 # name of book chosen 
    gutNum <- bookData[which(bookData$bookTitles == bookName),1] #gutenberg id chosen book 
    chosenBook <- gutenberg_download(gutNum) #gets book data 
    chosenBook %<>%
      unnest_tokens(word, text) %>% #isolates each word into its own row 
      anti_join(stop_words) # gets rid of the stop words 
    bkCount <-  chosenBook %>% count(word, sort = TRUE) #gets a word count 
    return(bkCount) #this is a dataframe 
  })
  
  ######### Sets up sentiment clouds for the two books - WORKS #########
  sentimentGraph1 <- reactive({
    if(input$option == "senti"){
    bkSent <- inner_join(wordFreqsOfChosen1(),snrc) 
    bkSent <- bkSent %>% group_by(sentiment) %>%
      summarise(freq = n()) %>% arrange(sentiment) %>%
      na.omit()
    daGraph <- ggplot(bkSent) +
      aes(x = sentiment, fill = sentiment, weight = freq) +
      geom_bar() +
      scale_fill_hue() +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45))+ 
      ggtitle(input$book1)+ theme(legend.position = "none")
    return(daGraph)
    }
  })

  output$senti1 <- renderPlot(sentimentGraph1())
  
  sentimentGraph2 <- reactive({
    if(input$option == "senti"){
      bkSent <- inner_join(wordFreqsOfChosen2(),snrc) 
      bkSent <- bkSent %>% group_by(sentiment) %>%
        summarise(freq = n()) %>% arrange(sentiment) %>%
        na.omit()
      daGraph <- ggplot(bkSent) +
        aes(x = sentiment, fill = sentiment, weight = freq) +
        geom_bar() +
        scale_fill_hue() +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45))+ 
        ggtitle(input$book2)+ theme(legend.position = "none")
      return(daGraph)
    }
  })
  
  output$senti2 <- renderPlot(sentimentGraph2())
  
  
  
  ######### Sets up clouds for the two books - WORKS #########
  makeCloud1 <- reactive({
    set.seed(1234)
    if(input$option == "cloud"){
      bookCloud <- wordFreqsOfChosen1() %>%
        with(wordcloud(word, n, max.words = 100))
      return(bookCloud)
    }
  })
  
  makeCloud2 <- reactive({
    set.seed(1234)
    if(input$option == "cloud"){
      bookCloud <- wordFreqsOfChosen2() %>%
        with(wordcloud(word, n, max.words = 100))
      return(bookCloud)
    }
  })
  

  output$cloud1 <- renderPlot(makeCloud1())
  output$cloud2 <- renderPlot(makeCloud2())
  
  ######### Sets up frequency tables #########
  top1 <- reactive({
    if(input$option == "cloud"){
      wordfreaks <- wordFreqsOfChosen1()[1:25, ]
      colnames(wordfreaks) <- c("Words", "Frequency")
      return(wordfreaks)
    }
  })
  
  top2 <- reactive({
    if(input$option == "cloud"){
      wordfreaks <- wordFreqsOfChosen2()[1:25, ]
      colnames(wordfreaks) <- c("Words", "Frequency")
      return(wordfreaks)
    }
  })
  
  output$freq1 <- renderTable(top1())
  output$freq2 <- renderTable(top2())
  
  ######### Sets up countries for the two books - WORKS #########
    countryData1 <- reactive({ 
      
    bkCount <- wordFreqsOfChosen1() #data set of word freqs
    bkCountries <- intersect(countries, bkCount$word)#list of countries in book
      
    #isolates countries of interest from word frequency
    bkCountryFreq <- bkCount %>% filter(word %in% bkCountries) %>%
      rename(country = word) #also renames words to country
    #makes nices labels for the map
    nameFreq <- paste0(bkCountryFreq$country, ":", bkCountryFreq$n, sep = " ")
    bkCountryFreq <- cbind(bkCountryFreq, nameFreq)
      
    #finds the countries of interest from main world data set
    bkMini <- world %>% filter(country %in% bkCountries)
    #combines info from world data set to the frequencies and labels
    bkFull <- left_join(bkMini,bkCountryFreq)
     
    #makes latitudes and longitudes doubles
    bkFull$lat <- as.double(bkFull[,3])
    bkFull$long <- as.double(bkFull[,4])
      
    return(bkFull)#this is a dataframe
    })
  
  countryData2 <- reactive({ 
    
    bkCount <- wordFreqsOfChosen2() #data set of word freqs
    bkCountries <- intersect(countries, bkCount$word)#list of countries in book
    
    #isolates countries of interest from word frequency
    bkCountryFreq <- bkCount %>% filter(word %in% bkCountries) %>%
      rename(country = word) #also renames words to country
    #makes nices labels for the map
    nameFreq <- paste0(bkCountryFreq$country, ":", bkCountryFreq$n, sep = " ")
    bkCountryFreq <- cbind(bkCountryFreq, nameFreq)
    
    #finds the countries of interest from main world data set
    bkMini <- world %>% filter(country %in% bkCountries)
    #combines info from world data set to the frequencies and labels
    bkFull <- left_join(bkMini,bkCountryFreq)
    
    #makes latitudes and longitudes doubles
    bkFull$lat <- as.double(bkFull[,3])
    bkFull$long <- as.double(bkFull[,4])
    
    return(bkFull)#this is a dataframe
  })
  
  ######### Sets up maps for the two books - WORKS #########
  
  
  #output$experiment <- renderTable(countryData2())
  
  finalMap1 <- reactive({
    if(input$option == "maps"){#if the user selects the map option
      mapData <- countryData1() #countries of interest for book
      theMap <- leaflet(mapData)%>%
        setView(lng = 0, lat = 0, zoom = 1)%>%
        addTiles()%>%
        addCircles(data = mapData, lat = ~lat, lng = ~long, weight = 8,
                   radius = 300, popup = ~as.character(nameFreq))
      return(theMap) #this is a list???
    }
  })
  
  finalMap2 <- reactive({
    if(input$option == "maps"){#if the user selects the map option
      mapData <- countryData2() #countries of interest for book
      theMap <- leaflet(mapData)%>%
        setView(lng = 0, lat = 0, zoom = 1)%>%
        addTiles()%>%
        addCircles(data = mapData, lat = ~lat, lng = ~long, weight = 8,
                   radius = 300, popup = ~as.character(nameFreq))
      return(theMap) #this is a list???
    }
  })
  
  output$map1 <- renderLeaflet({finalMap1()})
  output$map2 <- renderLeaflet({finalMap2()})
  
  
  
} #ENDS function()


#### SHINY run the damn thing #### 

shinyApp(ui2, server2)




##### Paradise Lost Explorations ####

#GETS THE WORD FREQUENCIES 
bookName = "Paradise Lost" # name of book chosen 
gutNum <- bookData[which(bookData$bookTitles == bookName),1] #gutenberg id chosen book 
chosenBook <- gutenberg_download(gutNum) #gets book data - is a dataframe
chosenBook %<>%
  unnest_tokens(word, text) %>% #isolates each word into its own row 
  anti_join(stop_words) # gets rid of the stop words 
bkCount <-  chosenBook %>% count(word, sort = TRUE) #gets a word count 

#### sentiment analysis experimentation ####
snrc <- get_sentiments("nrc")
bkSent <- inner_join(bkCount,snrc) 
bkSent <- bkSent %>% group_by(sentiment) %>%
  summarise(freq = n()) %>% arrange(sentiment) %>%
  na.omit()
View(bkSent)

ggplot(bkSent) +
  aes(x = sentiment, fill = sentiment, weight = freq) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90))+ 
  ggtitle(bookName)+ theme(legend.position = "none")

#### word cloud experimentation ####
# bookCloud <- bkCount %>%
#   with(wordcloud(word, n, max.words = 10))
# typeof(bookCloud) #this is Null ???? 
wordfreaks <- bkCount[1:30, ]
colnames(wordfreaks) <- c("Word", "Frequency")

#GETS THE COUNTRIES 
bkCountries <- intersect(countries, bkCount$word)#list of countries in book
#isolates countries of interest from word frequency
bkCountryFreq <- bkCount %>% filter(word %in% bkCountries) %>%
  rename(country = word) #also renames words to country
#makes nices labels for the map
cName = c(bkCountryFreq[1:length(bkCountryFreq$n),1] )
cFreq = c(bkCountryFreq[1:length(bkCountryFreq$n),2] )
nameFreq <- paste0(bkCountryFreq$country, ":", bkCountryFreq$n, sep = " ")
bkCountryFreq <- cbind(bkCountryFreq, nameFreq)

#finds the countries of interest from main world data set
bkMini <- world %>% filter(country %in% bkCountries)
#combines info from world data set to the frequencies and labels
bkFull <- left_join(bkMini,bkCountryFreq)

#makes latitudes and longitudes doubles
bkFull$lat <- as.double(bkFull[,3])
bkFull$long <- as.double(bkFull[,4])

#create an interactive map 
# plMap <- leaflet(bkFull)%>%
#   setView(lng = 0, lat = 0, zoom = 1)%>%
#   addTiles()%>%
#   addCircles(data = bkFull, lat = ~lat, lng = ~long, weight = 8,
#              radius = 300, popup = ~as.character(nameFreq))
# typeof(plMap)
# plMap













