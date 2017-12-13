#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(wordcloud)
library(plotly)
library(dplyr)
library(maps)
library(RColorBrewer)
netw<-readRDS("netw.rds")
farw<-readRDS("farw.rds")
nettl<-read.csv("nettl.csv",stringsAsFactors = FALSE)
fartl<-read.csv("fartl.csv",stringsAsFactors = FALSE)
netsenti<-readRDS("netsenti.rds")
farsenti<-readRDS("farsenti.rds")
netpoints<-readRDS("netpoints.rds")
farpoints<-readRDS("farpoints.rds")
map.data <- map_data("state") 

# Define UI for application that draws a histogram
ui<-fluidPage(titlePanel(fluidRow(
                    column(8, 
                           h3("MA615 Twitter Data Mining Project"),
                           h6("Presented by Ying Li. 
                              All the codes can be approached from", a(href="https://github.com/YingLi99/615-twitter-mining", "YingLi99 Github"))
                    ))),
                  navbarPage(title= "",
                             tabPanel("Introduction",
                                      hr(),
                                      p("The reason I am choosing Net A Porter and Farfetch is because I am interested in fashion. And from where I come from, there is no such online fashion retailer like these two, sO I find this unusual and fancy. Another reason is that I would like to work in industries like online fashion retailers doing data analysis work.

Those two online fashion retailers are all from British and basically they sell the same product, so I am wondering when people are considering buying clothes online, what is their altitude towards these two online fashion retailer.

Therefore, for my analysis, I will be focusing on text analysis which will show people's altitude toward these two shops and maps to show from where are people talking about each shop. And I will be extracting their official twitter account and do some basic analysis on it. Overall, there will be four main analtsis which are:map, sentiments, wordcloud, timelne.
")),
                             tabPanel("Pictures",
                                      hr(),
                                      p("All pictures are from", span("Net A Porter and Farfetch official website", style="color:red"), 
                                        "Click the link for more information!"), 
                                      a(href="https://www.net-a-porter.com/us/en/","Net A Porter official website"),
                                      a(href="https://www.farfetch.com/shopping/women/items.aspx","Farfetch official website"),
                                      mainPanel(imageOutput("netpic"),imageOutput("farpic"),
                                        br()
                                      )
                             ),
                             tabPanel("Maps",
                                      h4("Places where people are talking about NetAPorter and Farfetch"),
                                      p("Because Google only allows 2500 requests per day for geocode, these two maps are not perfect. However we can still get a rough
                                        idea on places where people are talking about", span("NetAPorter & Farfetch", style="color:red"), "through Twitter.
                                        Obviously, the people talking thses all from California and New York, because the New York and California are places of fashion."),
                                      hr(),
                                      sidebarLayout(
                                        sidebarPanel(
                                         selectInput(inputId = "mapinput",
                                                     label="Select online fashion retailer",
                                                     choices = c("Net A Porter","Farfetch"))
                                        ),
                                        mainPanel(plotOutput("maps"))
                                      )
                                     ),
                             tabPanel("Timeline",
                                      h4("Track offical twitter accounts of NetAPOrter & Farfetch on Twitter timeline."),
                                      p("Here we can track online fashion retailer's favorites and retweets on Twitter."),
                                      hr(),
                                      sidebarLayout(
                                        sidebarPanel(selectInput(inputId = "timelineinput",
                                                                 label="Select online fashion retailer",
                                                                 choices = c("Net A Porter","Farfetch"))),
                                        mainPanel(plotlyOutput("timeline"))
                                      )),
                             
                             tabPanel("Sentiment",
                                      h4("Sentiment Analysis"),
                                      p("Here you an track online fashion retailer and analyzes people's feelings towards them. For simplicity, only positive
                                        and negative measurement are shown here."),
                                      hr(),
                                      sidebarLayout(
                                        sidebarPanel(selectInput(inputId = "sentiinput",
                                                                 label="Select online fashion retailer",
                                                                 choices = c("Net A Porter","Farfetch"))
                                      ),
                                      mainPanel(plotOutput("sentiment")))
                                      ),
                             
                             tabPanel("Word Cloud",
                                      h4("Text analysis for NetAPorter & Farfetch"),
                                      p("Here I plot the words with respect to their frequencies, the bigger the words, the higher the frequency."),
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput(inputId = "wordinput",
                                                      label="Select online fashion retailer",
                                                      choices = c("Net A Porter","Farfetch")),
                                          sliderInput("freq", "Minimum Frequency:",
                                                      min = 1,  max = 50, value = 15),
                                          sliderInput("max", "Maximum Number of Words:", 
                                                      min = 1,  max = 300,  value = 100)),
                                        mainPanel(
                                          plotOutput("wordcloud")
                                        )
                                      )
                             )
                             )
                  
                  )

#server
server<-function(input, output){
  
  ##pic
  output$netpic<-renderImage({
    return(list(
      src="netaporter.PNG",
      width=600,
      height=500)) 
},deleteFile = FALSE)
  
  output$farpic<-renderImage({
    return(list(
      src = "farfetch.PNG",
      width = 600,
      height = 500))
  },deleteFile = FALSE)
  
  ### maps
    output$maps<-renderPlot({
        if(input$mapinput=="Net A Porter"){
          ggplot(map.data) + 
            geom_map(aes(map_id = region),  
                     map = map.data,  
                     fill = "white",             
                     color = "grey20", size = 0.25) + 
            expand_limits(x = map.data$long, y = map.data$lat) +            
            theme(axis.line = element_blank(),  
                  axis.text = element_blank(),  
                  axis.ticks = element_blank(),                     
                  axis.title = element_blank(),  
                  panel.background = element_blank(),  
                  panel.border = element_blank(),                     
                  panel.grid.major = element_blank(), 
                  plot.background = element_blank(),                     
                  plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
            geom_point(data = netpoints,             
                       aes(x = x, y = y), size = 1,  
                       alpha = 1/5, color = "red")+labs(title="Map for Net A Porter")
        }
        else{
          ggplot(map.data) + 
            geom_map(aes(map_id = region),  
                     map = map.data,  
                     fill = "white",             
                     color = "grey20", size = 0.25) + 
            expand_limits(x = map.data$long, y = map.data$lat) +            
            theme(axis.line = element_blank(),  
                  axis.text = element_blank(),  
                  axis.ticks = element_blank(),                     
                  axis.title = element_blank(),  
                  panel.background = element_blank(),  
                  panel.border = element_blank(),                     
                  panel.grid.major = element_blank(), 
                  plot.background = element_blank(),                     
                  plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
            geom_point(data = farpoints,             
                       aes(x = x, y = y), size = 1,  
                       alpha = 1/5, color = "blue")+labs(title="Map for Farfetch")
        }
      }
    )
  
  ##timeline
    
    output$timeline<-renderPlotly({
      if(input$timelineinput=="Net A Porter"){
        plot_ly(nettl, x = ~created, y = ~favoriteCount, name="Favorite", type = 'scatter', mode = 'lines', line=list(color="pink")) %>% add_trace(y=~retweetCount, name="Retweet", type = 'scatter', mode = 'lines', line=list(color="blue")) %>% add_trace(y=max(nettl$favoriteCount, nettl$retweetCount), type = 'scatter', mode = 'lines', line=list(color="purple"), hoverinfo = "text", text = ~text) %>% layout(
          title = 'Favorites/Retweets of Spotify Twitter NETAPORTER', xaxis = list(title = 'Date'), 
          yaxis=list(title='Number of favorites/retweets')) 
      }
      else{
        plot_ly(fartl, x = ~created, y = ~favoriteCount, name="Favorite", type = 'scatter', mode = 'lines', line=list(color="pink")) %>% add_trace(y=~retweetCount, name="Retweet", type = 'scatter', mode = 'lines', line=list(color="blue")) %>% add_trace(y=max(fartl$favoriteCount, fartl$retweetCount), type = 'scatter', mode = 'lines', line=list(color="purple"), hoverinfo = "text", text = ~text) %>% layout(
          title = 'Favorites/Retweets of Spotify Twitter Farfetch', xaxis = list(title = 'Date'), 
          yaxis=list(title='Number of favorites/retweets'))
      }
    }
    )
    
  ##sentiment
    output$sentiment<-renderPlot({
      if(input$sentiinput=="Net A Porter"){
        netsenti%>%
          group_by(sentiment) %>%
          top_n(10) %>%
          ungroup() %>%
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(word, n, fill = sentiment)) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = "Contribution to sentiment",
               x = NULL) +
          coord_flip()+labs(title="Sentiment Analysis for Net A Porter")
      }
      else{
        farsenti%>%
          group_by(sentiment) %>%
          top_n(10) %>%
          ungroup() %>%
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(word, n, fill = sentiment)) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = "Contribution to sentiment",
               x = NULL) +
          coord_flip()+labs(title="Sentiment Analysis for Farfetch")
      }
    }
    )
    
  ##word cloud
    output$wordcloud<-renderPlot({
      if(input$wordinput=="Net A Porter"){
        set.seed(1234)
        layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
        par(mar=rep(0, 4))
        plot.new()
        text(x=0.5, y=0.5, "Word Cloud for Net A Porter")
        wordcloud(words = netw$word, freq = netw$freq, scale=c(5,0.2), min.freq = input$freq, max.words=input$max,
                  random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(9,"Reds")[5:9])
      }
      else{
        set.seed(1234)
        layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
        par(mar=rep(0, 4))
        plot.new()
        text(x=0.5, y=0.5, "Word Cloud for Farfetch")
        wordcloud(words = farw$word, freq = farw$freq, scale=c(5,0.2), min.freq = input$freq, max.words=input$max,
                  random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2")[5:9])
      }
    }
    )
}
# Run the application 
shinyApp(ui = ui, server = server)

