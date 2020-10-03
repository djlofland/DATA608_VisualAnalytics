library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(RColorBrewer)
library(tidyverse)

# ===================================================================
# Load dataset
# ===================================================================

palette <- colorRampPalette(c("darkgreen", "white", "darkred"))

# ICD.Chapter, State, Year, Deaths, Population, Crude.Rate
df <- read.csv('https://raw.githubusercontent.com/djlofland/DATA608_VisualAnalytics/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')

# Since Crude.Rate is # per 100,000 population, need to calculate actual for changes
df <- df %>% 
  filter(ICD.Chapter != 'Codes for special purposes') %>%
  mutate(death_count = Crude.Rate * Population / 100000)

causes <- sort(unique(df$ICD.Chapter))
years <- sort(unique(df$Year))

# Calculate aggregates
nationalYear_df <- df %>%
  group_by(ICD.Chapter, Year) %>%
  summarize(us_rate = sum(death_count) / sum(Population) * 100000) %>%
  select(ICD.Chapter, Year, us_rate)

stateYear_df <- df %>%
  group_by(ICD.Chapter, State, Year) %>%
  summarize(Crude.Rate = Crude.Rate,
            st_rate = sum(death_count) / sum(Population) * 100000)

full_df <- full_join(stateYear_df, nationalYear_df, by=c('ICD.Chapter', 'Year'))

full_df <- full_df %>% mutate(
  st_chg = st_rate - lag(st_rate),
  us_chg = us_rate - lag(us_rate),
  delta = (st_chg - us_chg) )

# ===================================================================
# Layout Shiny Dashboard
# ===================================================================
ui = fluidPage(
  headerPanel('Explore CDC Mortality Data'),
  tabsetPanel(id="module3",
              tabPanel(title="Welcome", value="welcome", fluid = TRUE, 
                       sidebarLayout(position = "right",
                                     sidebarPanel(),
                                     mainPanel(
                                       withTags({
                                         div(h3('DATA 624 - Module 3'),
                                             h4('Donny Lofland - Oct 2, 2020'),
                                             a(href='https://github.com/djlofland/DS608_VisualAnalytics/tree/master/module3', "Dashboard Source Code on GitHub"),
                                             hr(),
                                             HTML("The <a href='https://wonder.cdc.gov/ucd-icd10.html'>CDC WONDER System</a> allows researchers 
                         to download mortatilty data from across the 50 states and DC.  This dashboard explores a subset 
                         of this data (<a href='https://github.com/charleyferrari/CUNY_DATA608/tree/master/module3/data'>here</a>).    
                         Two primary questions are to be explored in this Module:"),
                                             blockquote(
                                               h4('Question 1:'),
                                               p("As a researcher, you frequently compare mortality rates from particular causes across 
                           different States. You need a visualization that will let you see (for 2010 only) the 
                           crude mortality rate, across all States, from one cause (for example, Neoplasms, which 
                           are effectively cancers). Create a visualization that allows you to rank States by 
                           crude mortality for each cause of death."),
                                               h4('Question 2:'),
                                               p("Often you are asked whether particular States are improving their mortality rates (per cause) 
                           faster than, or slower than, the national average. Create a visualization that lets your clients 
                           see this for themselves for one cause of death at the time. Keep in mind that the national 
                           average should be weighted by the national population.")
                                             ),
                                             h5('Continue')
                                         )
                                       }),
                                       actionButton("jumpToExplore", "Explore Mortality Rates"),
                                       actionButton('jumpToChanges', 'Explore Mortality Changes')
                                     )
                       )
              ),
              tabPanel(title="Explore Mortality", value="explore", fluid = TRUE,
                       sidebarLayout(
                         sidebarPanel(
                           selectInput('cause', 'Cause', causes, selected='Certain conditions originating in the perinatal period'),
                           selectInput('year', 'Year', years, selected='2010')
                         ),
                         mainPanel(fluidRow(
                           plotlyOutput("plot1"))
                         )
                       )
              ),
              tabPanel(title="Explore Changes", value="changes", fluid = TRUE,
                       sidebarLayout(
                         sidebarPanel(
                           selectInput('cause2', 'Cause', causes, selected='Certain conditions originating in the perinatal period')
                           #selectInput('year2', 'Year', years, selected='2010')
                         ),
                         mainPanel(fluidRow(
                           plotlyOutput("plot2")),
                           sliderInput("year2", "Year:",
                                       min=2000, max=2010, value=2000, step=1,
                                       animate=animationOptions(100)
                           )
                         )
                       )
              )
  ))

# ===================================================================
# Shiny Interactive Backend
# ===================================================================
server <- function(input, output, session) {
  
  # ===================================================================
  # Handler for Button on Welcome page - switch to tabs
  # ===================================================================
  observeEvent(input$jumpToExplore, {
    updateTabsetPanel(session, "module3", selected = "explore")
  })
  
  observeEvent(input$jumpToChanges, {
    updateTabsetPanel(session, "module3", selected = "changes")
  })
  
  # ===================================================================
  # Render Explore Plot on drop down changes
  # ===================================================================
  output$plot1 <- renderPlotly({
    exp_years <- full_df %>% 
      filter(ICD.Chapter == input$cause) %>% 
      ungroup() %>%
      select(Year) %>%
      unique() %>% 
      pull() %>% 
      sort() 
    
    if (is.element(input$year, exp_years)) {
      year_default <- input$year
    } else {
      year_default <- head(exp_years, 1)
    }
    
    updateSelectInput(session, inputId = "year",
                      choices = exp_years,
                      selected = year_default)
    
    dfSlice <- full_df %>%
      filter(ICD.Chapter == input$cause, Year == year_default)
    
    if (nrow(dfSlice) == 0) {
      return
    }
    
    states <- sort(unique(dfSlice$State))
    
    fig1 <- plot_ly(dfSlice, x = ~Crude.Rate, y = ~reorder(states, Crude.Rate), 
                    height= 1000,
                    name = 'CDC Mortality Crude Rate (by State and Year)',
                    type = 'bar', 
                    orientation = 'h',
                    
                    marker = list(color = 'rgba(50, 96, 171, 0.6)',
                                  line = list(color = 'rgba(50, 96, 171, 1.0)', 
                                              width = 1))) 
    
    fig1 <- fig1 %>% layout(title = "CDC Mortality Rates (by State)",
                            margin=list(l=20, r=20, t=40, b=40),
                            yaxis = list(title='State',
                                         showgrid = FALSE, 
                                         showline = FALSE, 
                                         showticklabels = TRUE, 
                                         domain= c(0, 0.85)),
                            xaxis = list(title='Mortality (Crude Rate per 100,000 population)', 
                                         zeroline = FALSE, 
                                         showline = FALSE, 
                                         showticklabels = TRUE, 
                                         showgrid = TRUE)) 
    
    fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                     x = dfSlice$Crude.Rate * 1.1 ,  y = states,
                                     text = paste(round(dfSlice$Crude.Rate, 2), ''),
                                     font = list(family = 'Arial', size = 12, color = 'rgb(50, 96, 171)'),
                                     showarrow = FALSE)
    
    fig1
  })
  
  output$plot2 <- renderPlotly({
    full_df <- full_df
    df <- df
    
    exp_years2 <- full_df %>% 
      filter(ICD.Chapter == input$cause2) %>% 
      ungroup() %>%
      drop_na() %>%
      select(Year) %>%
      unique() %>% 
      pull() %>% 
      sort() 
    
    if (is.element(input$year2, exp_years2)) {
      year_default2 <- input$year2
    } else {
      year_default2 <- head(exp_years2, 1)
    }
    
    updateSliderInput(session, inputId = "year2",
                      min = min(exp_years2), max = max(exp_years2),
                      value = year_default2)
    
    dfSlice <- full_df %>%
      filter(ICD.Chapter == input$cause, Year == year_default2)
    
    states <- sort(unique(dfSlice$State))
    
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    fig <- plot_geo(dfSlice, locationmode = 'USA-states')
    fig <- fig %>% add_trace(
      z = ~delta, locations = ~State,
      color = ~delta, zauto=FALSE, zmin=~delta %>% min(), zmid=0, zmax=~delta %>% max(), 
      colors = 'PiYG' #palette(50)
    )
    
    fig <- fig %>% colorbar(title = "Relative Changes<br /> (0=US Avg)")
    fig <- fig %>% layout(
      title = 'Mortality Changes relative to US',
      geo = g
    )
    
    fig
  })
}

# ===================================================================
# Launch Shiny Dashboard Server
# ===================================================================
shinyApp(ui = ui, server = server)
