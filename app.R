if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rnaturalearth)) install.packages("rnaturalearth", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(gganimate)) install.packages("gganimate", repos = "http://cran.us.r-project.org")

#download csv file as ncov
ncov <- read.csv("https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv")

#download world dataset using natural earths

world <- ne_countries()
layer_id <- c(1:dim(world@data[1]))
world@data["layer_id"] = layer_id


#get all the country names
names_df <- ncov[2]
names_df <- names_df %>%
    distinct(Country.Region)

names2_df <- world@data[18] # names from world to compare with

# first add the new necessary levels to the dataframe before changing the country names to the one in the world dataset
levels(ncov$Country.Region) = c(levels(ncov$Country.Region), "United States", "Côte d'Ivoire", "Dem. Rep. Congo",
                                "Congo", "Central African Rep.", "Swaziland", "Czech Rep.",
                                "Bosnia and Herz.", "Macedonia", "Korea", "Taiwan")

## creating function for correcting names
original_name <- c("US", "Cote d'Ivoire", "Congo (Kinshasa)", "Congo (Brazzaville)",
                   "Central African Republic", "Eswatini", "Czechia", "Bosnia and Herzegovina",
                   "North Macedonia", "Taiwan*", "Korea, South", "Dominican Republic", "Equatorial Guinea")
new_name <- c("United States", "Côte d'Ivoire", "Dem. Rep. Congo", "Congo",  "Central African Rep.", 
              "Swaziland", "Czech Rep.", "Bosnia and Herz.", "Macedonia", "Taiwan", "Korea", "Dominican Rep.",
              "Eq. Guinea")

name_convert = function(country_name){
    if(country_name %in% original_name){
        # give back the name is the new_name vector
        new_country_name <- new_name[match(country_name, original_name)]
    }
    
    else{
        new_country_name <- as.character(country_name)
    }
    
    return(new_country_name)
}

ncov$Country.Region <- sapply(ncov$Country.Region, FUN = name_convert) 

#remove province since its not necessary
ncov1 <- ncov %>% 
    dplyr::select(-3)

#change country.region to "name"
names(ncov1)[2] <- "name"
names(ncov1) <- tolower(names(ncov1))
ncov1$date <- ymd(as.character(ncov1$date))

#add continent variable to the ncov1 dataset
name_cont <- world@data %>% select(name, continent)
ncov1 <- left_join(ncov1, name_cont, by = "name")


###Creating necessary dataframes

### create a continental dataframe

cont_data <- ncov1 %>%
    filter(date == max(date)) %>%
    group_by(continent) %>%
    summarise(conf = sum(confirmed, na.rm = TRUE) ,recov = sum(recovered, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE))
levels(cont_data$continent) = c(levels(cont_data$continent), "Other")

for (i in 1:nrow(cont_data)){
    if(is.na(cont_data$continent[i])){
        cont_data$continent[i] = "Other"
    }
}

# create a dataframe for the datatable
data_tab <- ncov1 %>%
    group_by(name) %>%
    summarise(Confirmed = max(confirmed, na.rm = T), Recovered = max(recovered, na.rm = T), Deaths = max(deaths, na.rm = T)) %>%
    arrange(desc(Confirmed))%>%
    mutate("Deaths(%)" = round(Deaths*100/Confirmed, digits = 2), "Recovery(%)" = round(Recovered*100/Confirmed, digits = 2))


#ncov2 to be use for plotting points(might be redundant)
ncov2 <- ncov1 %>% 
    group_by(date, name) %>%
    summarise(conf = sum(confirmed), recov = sum(recovered, na.rm = T), deaths = sum(deaths), lat = mean(lat), long = mean(long)) %>%
    mutate(current = conf - recov - deaths)

# daily world statistics dataframe
ncov_world <- ncov1 %>%
    group_by(date) %>%
    summarise(conf = sum(confirmed, na.rm = T), recov = sum(recovered, na.rm = T), deaths = sum(deaths, na.rm = T))

#creating a daily account of new cases, new deaths and new recoveries for barhraphs
#intial data was cumulative so it needs to change to make it give new cases only

deaths_bar <- ncov_world$deaths[1]
recov_bar <- ncov_world$recov[1]
conf_bar <- ncov_world$conf[1]
dates_bar <- as.Date(ncov_world$date[1])

for (i in 2: nrow(ncov_world)){
    deaths_bar <- c(deaths_bar, ncov_world$deaths[i]-ncov_world$deaths[i-1] )
    recov_bar <- c(recov_bar, ncov_world$recov[i]- ncov_world$recov[i-1] )
    conf_bar <- c(conf_bar, ncov_world$conf[i]-ncov_world$conf[i-1] )
    dates_bar <-  c(dates_bar, as.Date(ncov_world$date[i]))
}
#creating the dataframes

conf1 <- as.data.frame(conf_bar)
recov1 <- as.data.frame(recov_bar)
deaths1 <- as.data.frame(deaths_bar)
dates1 <- as.data.frame(dates_bar)
dates1$dates_bar <- ymd(as.character(dates1$dates_bar))

conf_daily <- bind_cols(dates1, conf1)
deaths_daily <- bind_cols(dates1, deaths1)
recov_daily <- bind_cols(dates1, recov1)

# to be used for creating the key for colouring regions
world@data <- left_join(world@data, data_tab, by = "name")


pal <- colorBin("Reds",data_tab,
                #putting data into groups
                bins = round(quantile(data_tab$Confirmed, probs = c(0, 0.3, 0.6, 0.71, 0.8, 0.9, 0.95, 1), na.rm = T))
)


#point size
s = 0.5


ui <- navbarPage(h4("Covid-19"),
                 tabPanel(h4("Country"),
                          setBackgroundColor("#dddddd"),
                          sidebarLayout(
                              sidebarPanel(width = 3,
                                           verticalLayout(
                                               selectInput(
                                                   "country", "Country", choices = world@data[["name"]], selected = "", multiple = FALSE),
                                               h1(textOutput("text4"), style = "color:#000066"),
                                               textOutput("text4a"),
                                               h1(textOutput("text5"), style = "color:#000066"),
                                               textOutput("text5a"),
                                               h1(textOutput("text6"), style = "color:#000066"),
                                               textOutput("text6a"),
                                               radioButtons("countryplot", "New cases", c("Confirmed" = "conf","Recovered" = "recov", "Deaths" = "deaths"), selected = "conf", inline = TRUE),
                                               plotlyOutput("plot10", height = "200px")
                                           )
                              ),
                              
                              mainPanel( width = 9,
                                         leafletOutput("mymap", width = "100%", height = "550px"),
                                         verticalLayout(
                                             
                                             fluidRow(
                                                 column(4,
                                                        h1(textOutput("text2"), style = "color:#000066")),
                                                 column(4,
                                                        h1(textOutput("text3"), style = "color:#000066")),
                                                 
                                                 column(4,
                                                        h1(textOutput("text1"), style = "color:#000066"))
                                             ),
                                             
                                             fluidRow(
                                                 
                                                 column(4,
                                                        textOutput("text4a1")),
                                                 
                                                 column(4,
                                                        textOutput("text5a1")),
                                                 
                                                 column(4,
                                                        textOutput("text6a1"))
                                                 
                                                 
                                             )
                                         )
                                         
                              )
                          )
                 ),
                 
                 tabPanel(h4("Statistical Graphs"),
                          selectInput("country2", "Country", choices = world@data[["name"]], selected = "", multiple = FALSE),
                          fluidRow(
                              column(4,
                                     plotlyOutput("plot1")),
                              column(4,
                                     plotlyOutput("plot2")),
                              column(4,
                                     plotlyOutput("plot3"))
                          )
                 ),
                 tabPanel(h4("World Data"),
                          fluidRow(
                              column(4,
                                     plotlyOutput("plot11")),
                              column(4,
                                     plotlyOutput("plot12")),
                              column(4,
                                     plotlyOutput("plot13"))
                          ),
                          
                          DTOutput("tbl"),
                          #world data division
                          fluidRow(
                              column(4,
                                     plotlyOutput("plot4")),
                              column(4,
                                     plotlyOutput("plot5")),
                              column(4,
                                     plotlyOutput("plot6")),
                              column(4,
                                     plotlyOutput("plot7")),
                              column(4,
                                     plotlyOutput("plot9")),
                              column(4,
                                     plotlyOutput("plot8"))
                          )
                 ),
                 theme = shinytheme("united")
                 
)


server <- function(input, output, session) {
    
    output$plot1 <- renderPlotly({
        data <- ncov2 %>% filter(name %in% input$country2)
        
        p <- ggplot(data, aes(x = date, y = conf)) + geom_line(colour = "#000066") + geom_point(size = s, colour = "#000066") + 
            xlab("Date") + ylab("Confirmed Cases") + theme(plot.background = element_rect(fill = "#dddddd"))  + scale_x_date(date_labels = "%d-%b")
        ggplotly(p)
    })
    
    output$plot2 <- renderPlotly({
        data <- ncov2 %>% filter(name %in% input$country2)
        p <- ggplot(data, aes(x = date, y = deaths)) + geom_line(colour = "#000066") + geom_point(size = s, colour = "#000066") + 
            xlab("Date") + ylab("Deaths") + theme(plot.background = element_rect(fill = "#dddddd")) + scale_x_date(date_labels = "%d-%b")
        ggplotly(p)
    })
    output$plot3 <- renderPlotly({
        data <- ncov2 %>% filter(name %in% input$country2)
        p <- ggplot(data, aes(x = date, y = recov)) + geom_line(colour = "#000066") + geom_point(size = s, colour = "#000066") +
            xlab("Date") + ylab("Recoveries") + theme(plot.background = element_rect(fill = "#dddddd")) + scale_x_date(date_labels = "%d-%b")
        ggplotly(p)
    })
    
    output$plot4 <- renderPlotly({
        data <- ncov_world
        p <- ggplot(data, aes(x = date, y = conf)) + geom_line(colour = "#000066") + geom_point(size = s, colour = "#000066") + 
            xlab("Date") + ylab("Confirmed Cases") + theme(plot.background = element_rect(fill = "#dddddd")) + scale_x_date(date_labels = "%d-%b")
        ggplotly(p)
    })
    
    output$plot5 <- renderPlotly({
        data <- ncov_world
        p <- ggplot(data, aes(x = date, y = recov)) + geom_line(colour = "#000066") + geom_point(size = s, colour = "#000066") + 
            xlab("Date") + ylab("Recoveries") + theme(plot.background = element_rect(fill = "#dddddd"))  + scale_x_date(date_labels = "%d-%b")
        ggplotly(p)
    })
    
    output$plot6 <- renderPlotly({
        data <- ncov_world
        p <- ggplot(data, aes(x = date, y = deaths) ) + geom_line(colour = "#000066") + geom_point(size = s, colour = "#000066") +
            xlab("Date") + ylab("Deaths") + theme(plot.background = element_rect(fill = "#dddddd"))  + scale_x_date(date_labels = "%d-%b")
        ggplotly(p)
    })
    
    output$plot7 <- renderPlotly({
        p <- ggplot(conf_daily, aes(dates_bar,conf_bar)) + geom_bar(position = "stack", stat = "identity", fill = "#000066") + 
            ylab("New Confirmed Cases") + xlab("Date") + theme(plot.background = element_rect(fill = "#dddddd"))  + scale_x_date(date_labels = "%d-%b")
        #p + transition_reveal(along = dates_bar)
        ggplotly(p)
    })
    
    output$plot8 <- renderPlotly({
        
        p <- ggplot(deaths_daily, aes(dates_bar,deaths_bar)) + geom_bar(position = "stack", stat = "identity", fill = "#000066") +
            ylab("New Death Cases") + xlab("Date") + theme(plot.background = element_rect(fill = "#dddddd"))  + scale_x_date(date_labels = "%d-%b")
        #p + transition_reveal(along = dates_bar)
        
        # p <- ggplot(deaths_daily) + geom_line(aes(dates_bar,deaths_bar))
        ggplotly(p)
    })
    
    output$plot9 <- renderPlotly({
        p <- ggplot(recov_daily, aes(dates_bar,recov_bar)) + geom_bar(position = "stack", stat = "identity", fill = "#000066") +
            ylab("New Recovery Cases") + xlab("Date") + theme(plot.background = element_rect(fill = "#dddddd"))  + scale_x_date(date_labels = "%d-%b")
        # p + transition_reveal(along = dates_bar)
        
        # p <- ggplot(recov_daily) + geom_line(aes(dates_bar,recov_bar))
        ggplotly(p)
    })
    
    output$plot10 <- renderPlotly({
        data <- ncov2 %>% filter(name %in% input$country)
        #create a variable that only takes the difference between
        #successive readings. This makes it easier to find new cases
        data$conf <- c(data$conf[1] ,diff(data$conf))
        data$recov <- c(data$recov[1] ,diff(data$recov))
        data$deaths <- c(data$deaths[1] ,diff(data$deaths))
        
        if(input$countryplot == "conf"){
            p <- ggplot(data, aes(x= date, y = conf)) +
                geom_bar(position = "stack", stat = "identity", fill ="#000066") + 
                ylab("New Cases") + xlab("Date") + theme(plot.background = element_rect(fill = "#f5f5f5"))  + scale_x_date(date_labels = "%d-%b")
            ggplotly(p)
        }
        
        else if (input$countryplot == "recov"){
            p <- ggplot(data, aes(x= date, y = recov)) +
                geom_bar(position = "stack", stat = "identity", fill = "#000066") + 
                ylab("New Cases") + xlab("Date") + theme(plot.background = element_rect(fill = "#f5f5f5"))  + scale_x_date(date_labels = "%d-%b")
            ggplotly(p)
        }
        
        else if (input$countryplot == "deaths"){
            p <- ggplot(data, aes(x= date, y = deaths)) +
                geom_bar(position = "stack", stat = "identity", fill = "#000066") + 
                ylab("New Cases") + xlab("Date") + theme(plot.background = element_rect(fill = "#f5f5f5"))  + scale_x_date(date_labels = "%d-%b")
            ggplotly(p)
        }
        
        
    })
    
    output$plot11 <- renderPlotly({
        trace1 <- list(
            hole = 0.8,
            type = "pie",
            labels = as.character(cont_data$continent),
            values = cont_data$conf,
            showlegend = TRUE
        )
        layout <- list(
            title = "Confirmed",
            paper_bgcolor="#dddddd")
        p <- plot_ly()
        p <- add_trace(p, hole=trace1$hole, type=trace1$type, labels=trace1$labels, values=trace1$values, showlegend=trace1$showlegend)
        p <- layout(p, title=layout$title, paper_bgcolor = layout$paper_bgcolor)
        p
    })
    
    output$plot12 <- renderPlotly({
        trace1 <- list(
            hole = 0.8,
            type = "pie",
            labels = as.character(cont_data$continent),
            values = cont_data$recov,
            showlegend = TRUE
        )
        layout <- list(
            title = "Recoveries",
            paper_bgcolor="#dddddd")
        p <- plot_ly()
        p <- add_trace(p, hole=trace1$hole, type=trace1$type, labels=trace1$labels, values=trace1$values, showlegend=trace1$showlegend)
        p <- layout(p, title=layout$title, paper_bgcolor = layout$paper_bgcolor)
        p
        
    })
    
    output$plot13 <- renderPlotly({
        trace1 <- list(
            hole = 0.8,
            type = "pie",
            labels = as.character(cont_data$continent),
            values = cont_data$deaths,
            showlegend = TRUE
        )
        layout <- list(
            title = "Deaths",
            paper_bgcolor="#dddddd")
        
        p <- plot_ly()
        p <- add_trace(p, hole=trace1$hole, type=trace1$type, labels=trace1$labels, values=trace1$values, showlegend=trace1$showlegend)
        p <- layout(p, title=layout$title, paper_bgcolor = layout$paper_bgcolor)
        p
    })
    
    output$text1 <- renderText({
        paste(comma(max(ncov_world$deaths)))
    })
    
    output$text2 <- renderText({
        paste( comma(max(ncov_world$conf)))
    })
    
    output$text3 <- renderText({
        paste(comma(max(ncov_world$recov)))
    })
    
    output$text4 <- renderText({
        data <- data_tab %>% filter(name %in% input$country)
        paste(comma(data$Confirmed))
    })
    
    output$text4a <- renderText({
        paste("Confirmed Cases")
    })
    
    output$text4a1 <- renderText({
        paste("Confirmed Cases")
    })
    
    output$text5 <- renderText({
        data <- data_tab %>% filter(name %in% input$country)
        paste(comma(data$Recovered))
    })
    
    output$text5a <- renderText({
        paste("Recoveries")
    })
    
    
    output$text5a1 <- renderText({
        paste("Recoveries")
    })
    
    output$text6 <- renderText({
        data <- data_tab %>% filter(name %in% input$country)
        paste(comma(data$Deaths))
    })
    
    output$text6a <- renderText({
        paste("Fatalities")
    })
    
    output$text6a1 <- renderText({
        paste("Fatalities")
    })
    
    output$tbl = renderDT(data_tab, options = list(lengthChange = TRUE))
    output$mymap <- renderLeaflet({
        leaflet(world, options = leafletOptions(
            attributionControl=FALSE)) %>%
            addPolygons(layerId = layer_id ,color = ~pal(Confirmed), weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,
                        highlightOptions = highlightOptions(color = "#000066", weight = 2,
                                                            bringToFront = TRUE), label = world@data$name) %>%
            
            addLegend( pal = pal, values = ~Confirmed,
                       title = "Confirmed Cases",
                       opacity = 0.5, position =  "bottomleft") %>%
            setView(0.5096,47.770345, zoom=2)
    })
    
    proxy <- leafletProxy("mymap")
    observe({
        if(input$country!=""){
            #get the selected polygon and extract the label point
            
            selected_polygon <- subset(world,world$name==input$country)
            polygon_labelPt <- selected_polygon@polygons[[1]]@labpt
            
            #remove any previously highlighted polygon
            proxy %>% removeShape("highlighted_polygon")
            
            #center the view on the polygon 
            proxy %>% setView(lng=polygon_labelPt[1],lat=polygon_labelPt[2],zoom=2)
            
            #add a slightly thicker red polygon on top of the selected one
            proxy %>% addPolylines(stroke=TRUE, weight = 4,color="#000066", fill = "#000066",data=selected_polygon,layerId="highlighted_polygon")
            
        }
        
    })
    
    observeEvent(input$mymap_shape_click, { # update the location selectInput on map clicks
        p <- input$mymap_shape_click
        if(!is.null(p$id)){
            
            updateSelectInput(session, "country", selected = world@data$name[as.integer(p$id)])
        }
    })
}

shinyApp(ui = ui, server = server)

