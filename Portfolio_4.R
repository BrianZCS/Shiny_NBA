library(ggplot2)
library(readr)
library(dplyr)
library(ggrepel)
library("tidyr")
library(shiny)
library(bslib)
library("rjson")
library(stringr)
library(tidyverse)

data = read_csv("https://pages.cs.wisc.edu/~zzheng/nba_2020_stats_cleaned.csv")
json_file <- "http://data.nba.net/data/10s/prod/v1/2019/players.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))$league$standard[1:510]
ID =  as.data.frame(do.call(rbind, json_data),encoding="UTF-8")
ID = ID %>% 
  select(temporaryDisplayName,personId)
names(ID)[names(ID) == "temporaryDisplayName"] = "Player"
names(ID)[names(ID) == "personId"] = "ID"


splits <- str_split_fixed(ID$Player, ", ", 2)
ID$Player <- paste(splits[,2], splits[,1], sep = ' ')
## Manually fix some name
ID$Player[ID$Player=="JJ Redick"] = "J.J. Redick"
data$Player[data$Player=="Danuel House"] = "Danuel House Jr."
data$Player[data$Player=="James Ennis"] = "James Ennis III"
data$Player[data$Player=="Lonnie Walker"] = "Lonnie Walker IV"
data$Player[data$Player=="Kevin Knox"] = "Kevin Knox II"
data$Player[data$Player=="Marcus Morris"] = "Marcus Morris Sr."


#setdiff(data$Player,ID$Player)
type = data %>% 
  select(ends_with("%")) %>% 
  colnames()

scatter <- function(x, dists,selected_) {
  x = x %>% 
    mutate(dist = dists) %>% 
    mutate(selected = selected_)
    
    ggplot(x) + 
      geom_point(aes(`3PA`,`3P%`,alpha = as.numeric(selected),
                                   size = as.numeric(selected)))+
      geom_point(data = x[x$dist == min(x$dist),], aes(`3PA`,`3P%`), col = 'red')+
      scale_alpha(range = c(0.1, .5), guide = FALSE) +
      scale_size(range = c(0.3, 1.2), guide = FALSE) +
      scale_color_brewer(palette = "Set2") +
      theme(panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "grey"))+
      labs(x="Three Points Attempts", y = "Three Points Percentge", caption = "Red: The player you choose")+
      ggtitle("Best Three Points Shooter in the NBA")+
      theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          plot.title = element_text(size=22))
}


overlay_histogram <- function(x, selected_) {
  sub_df <- filter(x, selected_)
  ggplot(x, aes(.data[[colnames(x)[2]]])) +
    geom_histogram(alpha = 0.3) +
    geom_histogram(data = sub_df,aes(.data[[colnames(x)[2]]])) +
    labs(x = colnames(x)[2])+
    ggtitle("Players' Statistics Histogram")+
    scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
    scale_fill_brewer(palette = "Set2", guide = "none")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          plot.title = element_text(size=22))
}

piegraph = function(x,dists){
  x = x %>% 
    mutate(dist = dists) %>% 
    filter(dist == min(dist))
  x%>% 
    select(ends_with("Proportion")) %>%
    select(-"2P Proportion") %>% 
    pivot_longer(cols =everything(),names_to = "proportion", values_to = "value") %>% 
    mutate(proportion = factor(proportion,levels = c("0-3 Proportion","3-10 Proportion","10-16 Proportion","16-3P Proportion"
                                                     ,"3P Proportion"))) %>% 
    ggplot(aes(x="", y=value, fill = proportion)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)+
    theme_void()+
    ggtitle(paste("Shooting proportion in each area for", x$Player))+
    theme(plot.title = element_text(size=19))
}

findname = function(x,dists){
  x = x %>% 
    mutate(dist = dists) %>% 
    filter(dist == min(dist))
    paste(x$Player, ", Position: ", x$Pos, ", Team: ", x$Tm, ", Age: ", x$Age, sep = "")
}

findURL = function(x, dists){
  x = x %>% 
    mutate(dist = dists) %>% 
    filter(dist == min(dist)) %>% 
    pull(Player)
    src = paste("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/",ID$ID[ID$Player == x],".png",sep = "")
}
reset_dist <- function(x, click) {
  nearPoints(x, click, allRows = TRUE, addDist = TRUE)$dist_
}

reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE,xvar = colnames(x)[2])$selected_
}

ui <- fluidPage(
  titlePanel("NBA Shooting Analysis"),
  tabsetPanel(
    tabPanel("Home Page",
             p("Author: Zhi Zheng (Brian),
                Email: zzheng94@wisc.edu,
               Education: UW-Madison, 2023er"),
             h2("Introduction"), 
             p("This project implements visualizations with interactions for users to learn and discover the basic summary statistics and interesting trends and habits about 3-point shooters in NBA for 2020 season. There are 3 major tabs. 
In the second tab. The first plot is a histogram showing the players' statistics and there's a selector for the x-axis of the histogram plot. Brushing this histogram will change the color of the dots in the scatter plot in the third tab. 
In the third tab, there are three visualizations. One is the scatter plot where users can click the point to select specific players and look at their statistics. The second is displaying the picture of player selected at the scatter plot. The third is a pie chart showing the shooting proportion in each area for the selected player. 
Below the plots lays a table showing the statistics of all the players the user query.
Hope you enjoy the app!"),
    ),
    tabPanel("Players' Statistics",
             fluidRow(
               column(6,img(src="https://pages.cs.wisc.edu/~zzheng/download.png"), selectInput("type", "Player Statistics", type)),
               column(6, plotOutput("hist", brush = brushOpts("plot_brush",direction = "x" )))),
             dataTableOutput("table"), 
    ),
    tabPanel("3-Point Shooters", 
             fluidRow(
               column(6, plotOutput("plot", click = "plot_click")),
               column(6, img(src="https://pages.cs.wisc.edu/~zzheng/download.png"))),
             textOutput("text"),
             fluidRow(
               column(6,  htmlOutput("picture")),
               column(6,  plotOutput("piechart"))
             ),
             dataTableOutput("player"),
             
    ),
  ),
  theme = bs_theme(
    fg = "#ff682c",
    bg = "#ffe6ec"
  ))

server <- function(input, output) {
  stat_subset <- reactive({
    data %>%
      select(Player,input$type)
  })
  point <- reactiveVal(c(0,rep(1, nrow(data)-1)))
  selected <- reactiveVal(rep(TRUE, nrow(data)))
  observeEvent(
    input$plot_brush,
    selected(reset_selection(stat_subset(), input$plot_brush))
  )
  observeEvent(
    input$plot_click,
    point(reset_dist(data, input$plot_click)),
  )
  output$text = renderText(paste("Player: ", findname(data,point())))
  output$plot <- renderPlot(scatter(data,point(),selected()))
  output$hist <- renderPlot(overlay_histogram(stat_subset(),selected()))
  output$player <- renderDataTable(data %>%  mutate(dist = point()) %>% 
                                     filter(dist == min(dist)))
  output$table = renderDataTable(filter(data, selected()))
  output$piechart = renderPlot(piegraph(data,point()))

  output$picture<-renderText({
    src = findURL(data,point())
    c('<img src="',src,'">')})
}

shinyApp(ui, server)

