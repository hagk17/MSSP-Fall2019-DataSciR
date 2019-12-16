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
booksId = sort(c(999999, 1232, 1129, 1122, 1120, 1118, 1112, 1004, 996, 981, 910, 832, 714, 514, 345, 341, 236, 228, 215, 203, 175, 174, 164, 161, 150, 147, 141, 140, 135, 113, 108, 103, 99, 98, 84, 77, 74, 73, 61, 57, 46, 45, 41, 35, 33, 30, 28, 20, 17, 16, 15))

numBooks = length(booksId)

miniMeta <- gutenberg_metadata %>% filter(gutenberg_id %in% booksId )

bookTitles <-as.vector(miniMeta$title)

bookData <- cbind(booksId, bookTitles)

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




#### specific to paradise lost experimenting ####

#Case Study: Paradise Lost

#gutNum <- 135

 # book <- gutenberg_download(gutNum)
 # book %<>%
 #   unnest_tokens(word, text) %>%
 #   anti_join(stop_words)
 # bkCount <-  book %>% count(word, sort = TRUE)
 # is.data.frame(bkCount)

# bkCountries <- intersect(countries, bkCount$word)#list of countries in book
# bkCountryFreq <- filter(bkCount, word %in% bkCountries) %>%
#   rename(country = word) #frequencies of countries
# nameFreq <- paste(bkCountryFreq$country, bkCountryFreq$n, sep = " ")
# bkCountryFreq <- cbind(bkCountryFreq, nameFreq)
# #data set of country, lat long and frequency specific to book
# bkMini <- world %>% filter(country %in% bkCountries)
# bkFull <- left_join(bkMini,bkCountryFreq)

# bkFull$lat <- as.double(bkFull$lat)
# bkFull$long <- as.double(bkFull$long)


#create an interactive map 
# plMap <- leaflet(bkFull)%>%
#   setView(lng = 0, lat = 0, zoom = 1)%>%
#   addTiles()%>%
#   addCircles(data = bkFull, lat = ~lat, lng = ~long, weight = 8,
#              radius = 300, popup = ~as.character(nameFreq))
# typeof(plMap)
# plMap


#### word cloud experimentation ####
# bookCloud <- bkCount %>%
#   with(wordcloud(word, n, max.words = 10))
# typeof(bookCloud) #this is Null ???? 


#### sentiment analysis experimentation ####

#### reddit scraping experimentation ####
# library(tidyverse)
# library(RedditExtractoR)
# library(magrittr)
# plReddit <- reddit_urls(search_terms="Sherlock",
#                   subreddit = "literature",
#                   sort_by = "new")
# View(plReddit)

# chosenBook %<>%
#   unnest_tokens(word, text) %>%
#   anti_join(stop_words)
# bkCount <-  chosenBook %>% count(word, sort = TRUE)

#plReddit$title 
#wordcloud(plReddit$title, max.words = 100)


#### SHINY User Interface ####
ui2 <- dashboardPage(
  dashboardHeader(title = "Literary Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      radioButtons("option", "What Do You want to Explore?",
                   c(
                     "Sentiments" = "sentiment",
                     "Word Cloud" = "cloud",
                     "Maps!" = "maps")),
      selectInput("book", "Take a look in this book", bookTitles) #ENDS selectInput
      
    )#ENDS sidebarMenu 
  ),#ENDS dashboardSidebar
  
  
  dashboardBody(
    
    mainPanel(leafletOutput("mapPlot"), #output for map 
              plotOutput("cloudy")) #output for cloud 
    #ENDS mainPanel
    
  )#ENDS dashboardBody
)



#### SHINY Server ####
server2 <- function(input, output) {
  
  
  ######### Sets up the server stuff for the maps ####################
  wordFreqsOfChosen <- reactive({
    bookName = input$book # name of book chosen 
    gutNum <- bookData[1,bookData$bookTitles == bookName] #gutenberg id chosen book 
    chosenBook <- gutenberg_download(gutNum) #gets book data 
    chosenBook %<>%
      unnest_tokens(word, text) %>% #isolates each word into its own row 
      anti_join(stop_words) # gets rid of the stop words 
    bkCount <-  chosenBook %>% count(word, sort = TRUE) #gets a word count 
    return(bkCount) #this is a dataframe 
  })
  
  countryData <- reactive({ 
    
    bkcount <- wordFreqsOfChosen() #data set of word freqs
    bkCountries <- intersect(countries, bkcount[,2])#list of countries in book 
    
    #isolates countries of interest from word frequency 
    bkCountryFreq <- bkcount %>% filter(word %in% bkCountries) %>%
      rename(country = word) #also renames words to country 
    #makes nices labels for the map 
    nameFreq <- paste(bkCountryFreq[,1], bkCountryFreq[,2], sep = " ")
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
  
  # finalMap <- reactive({
  #   if(input$option == "maps"){#if the user selects the map option
  #     mapData <- countryData() #countries of interest for book
  #     theMap <- leaflet(mapData)%>%
  #       setView(lng = 0, lat = 0, zoom = 1)%>%
  #       addTiles()%>%
  #       addCircles(data = mapData, lat = ~lat, lng = ~long, weight = 8,
  #                  radius = 300, popup = ~as.character(nameFreq))
  #     return(theMap) #this is a list???
  #   }
  # })
  
  #output$mapPlot <- renderLeaflet({ finalMap() })
  
  output$mapPlot <- renderLeaflet({
    if(input$option == "maps"){#if the user selects the map option
    leaflet(countryData())%>%
      setView(lng = 0, lat = 0, zoom = 1)%>%
      addTiles()%>%
      addCircles(data = countryData(), lat = ~lat, lng = ~long, weight = 8, 
                 radius = 300, popup = ~as.character(nameFreq))
    } 
  })
  
  # ERROR: $ operator is invalid for atomic vectors 
  #it might be because the typeof() returns a list based on above experimenting???

  
  
  ######### Sets up the server stuff for the cloud ####################
  makeCloud <- reactive({
    set.seed(1234)
    if(input$option == "cloud")
      bookCloud <- wordFreqsOfChosen() %>%
        with(wordcloud(word, n, max.words = 100))
    return(bookCloud)
  })
  
  # ERROR: $ operator is invalid for atomic vectors 
  #typeof() returns NULL for word cloud in experimentation above???? 
  output$cloudy <- renderPlot({makeCloud()})
  
  
} #ENDS function()


#### SHINY run the damn thing #### 

shinyApp(ui2, server2)
















