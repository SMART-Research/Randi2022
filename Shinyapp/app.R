# Required Libraries

library(shiny)
library(shinythemes)
library(thematic)
library(data.table)
library(ggplot2)
library(plotly)
library(GGally)
library(dplyr)


thematic_shiny(font = "auto")

not_sel <- "Not Selected"

about_page <- tabPanel(
    title = "About",
    br(),
   "This is a reproducible shiny app which is developed for exploring the data set initially.",
   br(),
   "The code behind this dashboard can be downloaded from the", a("GitHub.", href="https://github.com/SMART-Research/Randi2022"),
   br(),
   br(),
   "The following reference format is used to build this shiny app:",
   br(),
   a("How to Build a Data Analysis App in R Shiny", href="https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7")
   )
    
main_page <- tabPanel(
    title = "Visualization",
    sidebarLayout(
        sidebarPanel(
            title = "Inputs",
            fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
            selectInput("num_var_1", "Numerical Variable 1", choices = c(not_sel)),
            selectInput("num_var_2", "Numerical Variable 2", choices = c(not_sel)),
            selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
            selectInput("bins","Number of bins",choices = 30),
            
            br(),
            actionButton("run_button", "Run Analysis", icon = icon("play"))
            
        ),
        mainPanel(
            
            tabsetPanel(
                tabPanel(
                    title = "Overview",
                    fluidRow(
                        column(width = 6,
                               h4("Summary Statistics"),
                               verbatimTextOutput("summary_1")),
                        column(width = 6,
                               h4("Correlation Matrix Plot"),
                               plotOutput("vis") )
                    )),
                tabPanel(
                    title = "Visualization of Variables",
                    fluidRow(
                        column(width = 4,
                               h4("Distribution of Numeric Variable1"),
                               plotOutput("plot_1")),
                        column(width = 4,
                               h4("Distribution of Numeric Variable2"),
                               plotOutput("plot_2")),
                        column(width = 4,
                               h4("Compostion of the Class Variable"),
                               plotOutput("plot_3"))
                        
                    )
                ),
                tabPanel(
                    title = "Relationships",
                    fluidRow(h4("Relationship between Selecetd Variables"),
                             plotOutput("plot_4", height = "35em")
                    )
                )
                
            )
            
            
            
            
        )
    )
)

# Histogram for selected first numeric variable

draw_plot_1 <- function(data_input, num_var_1, bins){
    if(num_var_1 != not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_1)) +
            geom_histogram(bins = bins, color= "white" , fill = "coral")
    }
}

# Histogram for selected second numeric variable

draw_plot_2 <- function(data_input, num_var_2, bins){
    if(num_var_2 != not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_2)) +
            geom_histogram(bins = bins, color= "white", fill = "goldenrod1")
    }
}

# Bar plot for selected factor variable

draw_plot_3 <- function(data_input, fact_var){
    if(fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = fact_var)) +
            geom_bar(fill = "seagreen")
    }
}

# Plot for visualize the relationships among variables

draw_plot_4 <- function(data_input, num_var_1, num_var_2, fact_var){
    if(fact_var!=not_sel){
        data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
    }
    if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
            geom_point() # Distribution of two numeric variables wrt the factor variable
        
    }
    
    else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_1, y = num_var_2)) +
            geom_point() # Distribution of two numeric variables 
    }
    else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = fact_var, y = num_var_1, col = fact_var)) +
            geom_boxplot() # Distribution of one numeric variable wrt the factor variable
    }
    else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = fact_var, y = num_var_2, col = fact_var)) +
            geom_boxplot() # Distribution of one numeric variable wrt the factor variable
    }
}


ui <- navbarPage(
    title = "Expolatory Data Analysis",
    theme = shinytheme(theme = "darkly"),
    main_page,
    about_page
)

server <- function(input, output){
    
    data_input <- reactive({
        req(input$csv_input)
        fread(input$csv_input$datapath)
    })
    
observeEvent(data_input(),{
        num <- select_if(data_input(), is.numeric)
        fac <- select_if(data_input(), is.character)
        choices1 <- c(not_sel,names(num))
        choices2 <- c(not_sel,names(fac))
        choices3 <- c(10, 20, 30, 40, 50)
        updateSelectInput(inputId = "num_var_1", choices = choices1)
        updateSelectInput(inputId = "num_var_2", choices = choices1)
        updateSelectInput(inputId = "fact_var", choices = choices2)
        updateSelectInput(inputId = "bins", choices = choices3)
        
        
    })
    
    num_var_1 <- eventReactive(input$run_button,input$num_var_1)
    num_var_2 <- eventReactive(input$run_button,input$num_var_2)
    fact_var <- eventReactive(input$run_button,input$fact_var)
    bins <- eventReactive(input$run_button,input$bins)
    
    # plot
    
    plot_1 <- eventReactive(input$run_button,{
        draw_plot_1(data_input(), num_var_1(), bins())
    })
    
    output$plot_1 <- renderPlot(plot_1())
    
    plot_2 <- eventReactive(input$run_button,{
        draw_plot_2(data_input(), num_var_2(), bins())
    })
    
    output$plot_2 <- renderPlot(plot_2())
    
    plot_3 <- eventReactive(input$run_button,{
        draw_plot_3(data_input(), fact_var())
    })
    
    output$plot_3 <- renderPlot(plot_3())
    
    
    plot_4 <- eventReactive(input$run_button,{
        draw_plot_4(data_input(), num_var_1(), num_var_2(), fact_var())
    })
    
    output$plot_4 <- renderPlot(plot_4())
    
    summary_1 <- eventReactive(input$run_button,{
        summary(data_input())
    })
    
    output$summary_1 <- renderPrint(summary_1())
    
    vis <- eventReactive(input$run_button,{
        ggcorr(data_input(), label = TRUE)
    })
    
    output$vis <- renderPlot(vis())
    
    
}

shinyApp(ui = ui, server = server)

