library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

dataset <- read.csv("international_undergraduate_tuition_data.csv")
tuition <- dataset[, c("REF_DATE", "GEO", "Field.of.study", "VALUE")] %>%
  rename(year = REF_DATE, province = GEO, subject = Field.of.study, tuition_fee = VALUE) %>%
  filter(province != "Canada" & province != "Yukon")

tuition$year <- substr(tuition$year, 1, 4)

# App Starts Here
ui <- fluidPage(
  titlePanel("Undergraduate Tuition Trends for International Students by Provinces 
             and Disciplines"),
  p(strong("Description:"),"This App shows the trend of undergraduate tuition for 
    international students by province in different subject areas over multiple 
    years. Please select your interested subject and year range on the left, then 
    a line graph will display the corresponding trends, with each colored line 
    representing a province, referring to the legend on the right."),
  p(strong("Example of plot reading:"), "When a year range of", span("\"2008 
    to 2018\"", style = "color:blue"), "and the subject",  span("\"Mathematics, 
    computer and information sciences\"", style = "color:blue"), "and is selected, 
    the line graph on the right will illustrate the undergraduate tuition fee trend 
    for the Mathematics and CS program from 2008 to 2018 for all provinces in Canada. 
    The line graph presents a moderate rising trend for the majority of provinces, 
    a rapid growth for the province of Ontario, and a stable flat trend for Newfoundland 
    and Labrador from 2008 to 2017, then a slight increase from 2017 to 2018. It is 
    insteresting to see that Ontario and Quebec has the highest undergraduate tuition 
    fee for international students, while Newfoundland and Labrador and Prince Edwards 
    Island has the lowest international undergraduate tuition fee. To conclude, for an 
    international student studying math and computer science and seeking for cheap 
    tuition, the Atlantic provinces are better choices than Ontario and Quebec."),
  p(strong("Note:"),"1. Please ensure the ending year number is larger than starting 
  year number, otherwise the line graph will be blank."),
  p("2. There exist some missing values for certain provinces in certain fields of
    study, to avoid discontinuous trend line, the median is used to replace the 
    missing values."),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        # Add start year and end year select bar.
        column(width = 5, selectInput("start_year", "Starting year:",
                                      choices = unique(tuition$year), 
                                      selected = min(tuition$year))),
        column(width = 5, selectInput("end_year", "Ending year:", 
                                      choices = unique(tuition$year), 
                                      selected = max(tuition$year)))
      ),
      # Add radio buttons to select field of study.
      radioButtons("subject","Select interested field of study:",
                   choices = unique(tuition$subject),
                   selected = "Total, field of study"),
      width = 3
    ),  
    mainPanel(plotOutput("linegraph", width = "110%", height = 800))
  ))

server <- function(input, output) {
  output$linegraph <- renderPlot({
    # Create a new data frame filtered by the input values.
    tuition <- filter(tuition, year >= input$start_year 
                             & year <= input$end_year 
                             & subject == input$subject) %>%
      # Group by province to replace missing values with median.
      # To avoid discontinuous trends.
      group_by(province)%>%
      mutate(tuition_fee = ifelse(is.na(tuition_fee), 
                                  median(tuition_fee, na.rm = T), tuition_fee))
    
    # Line graph
    ggplot(tuition,
           aes(x = year, y = tuition_fee, colour = province, group = province)) +
      geom_line() + 
      geom_point() + 
      labs(title = "Tuition Fee Trend Over Years", x = "Year", y = "Tuition Fee") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0.5), 
            axis.title = element_text(size = 17), 
            axis.text = element_text(size = 13), 
            legend.text = element_text(size = 13), 
            legend.title = element_text(size = 15)) + 
      scale_color_manual(values=c("blue", "coral2", "gold2", "green3", 
                                  "cyan3", "firebrick3", "darkviolet", 
                                  "burlywood4", "lightpink3", "black"))
  })
}

shinyApp(ui, server)
