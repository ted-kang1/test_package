library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(shinyjs)
library(tidyverse)
library(shinyWidgets)


city_crimes <- "C:/Users/tedka/Dropbox (Princeton)/VIP (Data) - Violence and Inequality Project/AV_Website/OUTPUT/city/city_all_crimes-2023-07-15.csv"
place_info <- "C:/Users/tedka/Dropbox (Princeton)/VIP (Data) - Violence and Inequality Project/AV_Website/_murders/HELPERS/data/place_info_files/PlaceInfoGT10000.rds"

city_df <- readr::read_csv(city_crimes)
PLACE_INFO2010_DF <- readRDS(place_info)
test <- merge(city_df,PLACE_INFO2010_DF,by="id", all.x=TRUE)
test <- test[order(test$place_name,test$year, test$month),]
rownames(test) <- 1:nrow(test)

# Crime id
test$crime_type[test$crime_type==1] <- "murder"
test$crime_type[test$crime_type==300] <- "nonfatal"
test$crime_type[test$crime_type==400] <- "fatal"

test_year <-  test %>% filter(month==0 & year>=2014) %>% select(id, year, month, place_name,crime_type,source_id, crime_count)
test_year <- test_year %>% group_by(id, year, month, place_name,crime_type) %>% summarise_at(vars(crime_count), list(crime_count=mean))
city <- unique(test_year$id) 

#create cityname to be saved as file name
cityname <- unique(test_year$place_name)

test_year_spread <- test_year %>%
    spread(key = crime_type, value = crime_count)
test_year_spread$year <- as.integer(test_year_spread$year)
test_year_spread$total <- test_year_spread$fatal + test_year_spread$nonfatal


####################################################### ytd

cities_list <- read.csv("C:/Users/tedka/Dropbox (Princeton)/VIP (Data) - Violence and Inequality Project/AV_Website/AV_cities_list.csv")
sources <- read.csv("C:/Users/tedka/Dropbox (Princeton)/VIP (Data) - Violence and Inequality Project/AV_Website/_murders/03_AVORG_COMPILER/data/source_id.csv")

# join with mapping files to get more info
# murder data
ucr <- read.csv("C:/Users/tedka/Dropbox (Princeton)/VIP (Data) - Violence and Inequality Project/AV_Website/_murders/OUTPUT/crime_data.csv")
ucr <- ucr %>%
    left_join(cities_list, by=c("id"="stpl_fips")) %>%
    left_join(sources, by="source_id")

ucr_full <- read.csv("C:/Users/tedka/Dropbox (Princeton)/VIP (Data) - Violence and Inequality Project/AV_Website/_murders/01_CHECK_FILE_UPDATES/refresh_UCR/output/ucr1990_2022.csv") %>%
    select(-city) %>%
    left_join(cities_list, by=c("id"="stpl_fips")) %>%
    filter(year >= min(ucr$year), year <= max(ucr$year))

recent_year <- max(ucr$year)

max_month_this_year <- ucr %>% filter(year == recent_year) %>% pull(month) %>% max(.)
ucr_ytd <- ucr %>%
    filter(year == recent_year | year == (recent_year-1), month!=0, month <= max_month_this_year) %>%
    select(-source_id, -population_est)

ucr_ytd_current <- ucr_ytd %>%
    filter(year == recent_year) %>%
    group_by(id, city, year) %>%
    summarize(crime_count = sum(crime_count))
ucr_ytd_prev <- ucr_ytd %>%
    filter(year == (recent_year-1)) %>%
    group_by(id, city, year) %>%
    summarize(crime_count = sum(crime_count))


ytd_df <- ucr_ytd_prev %>%
    full_join(ucr_ytd_current, by=c("city","id"), suffix=c("_previous", "_new")) %>%
    mutate(YOY_ytd_percent_change = ((crime_count_new - crime_count_previous)/crime_count_previous)*100) %>%
    select(city, id, 
           !!sym(paste0("ytd_", recent_year)):=crime_count_new, 
           !!sym(paste0("ytd_", (recent_year-1))):=crime_count_previous,
           YOY_ytd_percent_change)


a <- merge(test_year_spread, ytd_df, by = "id")
combined_df <- a[-c(9, 3)]
combined_df <- combined_df[order(combined_df$place_name, combined_df$year),]
long_df <- tidyr::pivot_longer(combined_df, c("fatal", "murder", "nonfatal", "total"))

####################################################### ytd


ui <- page_sidebar(
    theme = bs_theme(bootswatch = "minty"),
    sidebar = sidebar(
        # varSelectInput("xvar", "X variable", c("year"), selected = "year"),
        pickerInput("yvar", "Y variable", choices = c("fatal", "murder", "nonfatal", "total"), selected = "fatal", options =list("actions-box" = TRUE), multiple = T),
        selectInput("type", "Metric", choices = list("Year to Year", "Year to Date"), selected = "Year to Year"),
        pickerInput(
            "placename", "Filter by City",
            choices = unique(combined_df$place_name), 
            selected = "Chicago city",
            multiple = TRUE
        ),
        hr(), # Add a horizontal rule
        checkboxInput("outliers", "Show Only Outliers", FALSE)
    ),
    plotOutput("col")
)

server <- function(input, output, session) {
    subsetted <- reactive({
        req(input$placename)
        if (input$outliers) {
            long_df %>% filter(place_name %in% input$placename) %>%
                filter((murder/nonfatal > 2) | (fatal/nonfatal > 2), (murder > 10) | (fatal > 10))
        } else {
            long_df |> filter(place_name %in% input$placename) %>% filter(name %in% input$yvar)
        }
    })
    
    output$col <- renderPlot({
        if (input$type == "Year to Year") {
            p <- ggplot(subsetted(), aes(year, y = value, color = name, group = name)) + list(
                theme(legend.position = "bottom"),
                aes(color = place_name),
                geom_line(linewidth = 2)
            ) 
        } else {
            p <- ggplot(subsetted(), aes(x = !!input$xvar, y = !!input$yvar)) + list(
                theme(legend.position = "top"),
                aes(color = place_name),
                geom_density()
            )
        }
        
        p
    }, res = 100)
}

shinyApp(ui, server)



###############################################################

df_list <- list(ytd_df, test_year_spread)

ui <- fluidPage(
    titlePanel("Simple app"),
    useShinyjs(),
    sidebarLayout(
        
        sidebarPanel(
            
            selectInput("data", "Choose a database",
                        choices=df_list, selected=df_list[[1]]),
            selectInput("xcol", "Variable X", c("a", "b", "c")),
            selectInput("ycol", "Variable Y", c("a", "b", "c"))),
        
        mainPanel(
            
            plotOutput(outputId = "plot")
            ,DTOutput("t1")
        )
    )
)

server <- function(input, output, session) {
    
    mydata <- eventReactive(input$data, {
        get(input$data)
    })
    
    observeEvent(input$data, {
        req(mydata())
        choices <- names(mydata())
        updateSelectInput(session,"xcol",choices = choices, selected=choices[1])
        updateSelectInput(session,"ycol",choices = choices, selected=choices[2])
    }, ignoreNULL = FALSE)
    
    output$t1 <- renderDT({mydata()})
    
    output$plot <- renderPlot({
        req(mydata(),input$xcol,input$ycol)
        if (is.null(mydata()) | !(input$xcol %in% colnames(mydata())) | !(input$ycol %in% colnames(mydata())) ) {
            return(NULL)
        } else{
            selected_df <- mydata() %>% select(input$xcol, input$ycol)
            plot(selected_df)
        }
        
    })
}

shinyApp(ui, server)
