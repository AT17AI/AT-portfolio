#library
library(ggplot2)
library(plotly)
library(shiny)

#import

df <- Absenteeism_at_work

#data exploration

#view data
head(df)
#view headers
colnames(df)
#number of coloumnes
ncol(df)
#number of rows
nrow(df)
#summary 
summary(df$`Absenteeism time in hours`)
#
str(df)


#data cleaning

#missing values
colSums(is.na(df))#identification
sum(is.na(df))#no  missing values

#outliers
numeric <- df[, sapply(df, is.numeric)] #group numeric coloumns
boxplot(numeric, las = 2)#use boxplots to show which have outliers
ggplot(data=df , aes(x=`Transportation expense`)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=`Service time`)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=Age)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=`Work load Average/day`)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=`Hit target`)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=`Disciplinary failure`)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=Education)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=`Social smoker`)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=Pet)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=Height)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=`Absenteeism time in hours`)) + geom_boxplot(outlier.colour = "red")

#data errors
# data is numeric no spelling mistakes
#check for negatives
sapply(df, function(x) any(x < 0, na.rm = TRUE))

#remove redundant coloumn (ID)
df$ID <- NULL


names(df) <- make.names(names(df))

numeric_cols <- names(df)[sapply(df, is.numeric)]

predictor_cols <- setdiff(
  numeric_cols,
  "Absenteeism.time.in.hours"
)

#encoding
df$Reason.for.absence <- as.factor(df$Reason.for.absence)
df$Month.of.absence <- as.factor(df$Month.of.absence)
df$Day.of.the.week <- as.factor(df$Day.of.the.week)
df$Seasons <- as.factor(df$Seasons)
df$Education <- as.factor(df$Education)

df$Disciplinary.failure <- as.factor(df$Disciplinary.failure)
df$Social.drinker <- as.factor(df$Social.drinker)
df$Social.smoker <- as.factor(df$Social.smoker)

# =========================
# UI
# =========================

ui <- fluidPage(
  
  titlePanel("Absenteeism at Work Analysis Dashboard"),
  
  tabsetPanel(
    
    # =====================
    # SCATTER PLOT
    # =====================
    
    tabPanel("Scatter Plot",
             
             fluidRow(
               
               column(
                 4,
                 selectInput(
                   "x_var_1",
                   "X Variable:",
                   choices = names(df),
                   selected = "Age"
                 )
               ),
               
               column(
                 4,
                 selectInput(
                   "y_var_1",
                   "Y Variable:",
                   choices = names(df),
                   selected = "Absenteeism.time.in.hours"
                 )
               ),
               
               column(
                 4,
                 selectInput(
                   "color_var_1",
                   "Color By:",
                   choices = c("None", names(df)),
                   selected = "None"
                 )
               )
             ),
             
             hr(),
             
             plotOutput("plot1")
    ),
    
    # =====================
    # LINE PLOT
    # =====================
    
    tabPanel("Line Plot",
             
             fluidRow(
               
               column(
                 4,
                 selectInput(
                   "x_var_2",
                   "X Variable:",
                   choices = names(df),
                   selected = "Month.of.absence"
                 )
               ),
               
               column(
                 4,
                 selectInput(
                   "y_var_2",
                   "Y Variable:",
                   choices = names(df),
                   selected = "Absenteeism.time.in.hours"
                 )
               ),
               
               column(
                 4,
                 selectInput(
                   "group_var_2",
                   "Group By:",
                   choices = c("None", names(df)),
                   selected = "None"
                 )
               )
             ),
             
             hr(),
             
             plotOutput("plot2")
    ),
    
    # =====================
    # HISTOGRAM
    # =====================
    
    tabPanel("Histogram",
             
             fluidRow(
               
               column(
                 4,
                 selectInput(
                   "hist_var",
                   "Variable:",
                   choices = names(df),
                   selected = "Absenteeism.time.in.hours"
                 )
               ),
               
               column(
                 4,
                 sliderInput(
                   "bins",
                   "Bins:",
                   min = 5,
                   max = 50,
                   value = 20
                 )
               ),
               
               column(
                 4,
                 selectInput(
                   "facet_var_3",
                   "Facet By:",
                   choices = c("None", names(df)),
                   selected = "None"
                 )
               )
             ),
             
             hr(),
             
             plotOutput("plot3")
    ),
    
    # =====================
    # BOXPLOT
    # =====================
    
    tabPanel("Boxplot",
             
             fluidRow(
               
               column(
                 4,
                 selectInput(
                   "y_var_4",
                   "Y Variable:",
                   choices = names(df),
                   selected = "Absenteeism.time.in.hours"
                 )
               ),
               
               column(
                 4,
                 selectInput(
                   "x_var_4",
                   "Group By:",
                   choices = names(df),
                   selected = "Seasons"
                 )
               ),
               
               column(
                 4,
                 checkboxInput(
                   "show_points",
                   "Show Points",
                   FALSE
                 )
               )
             ),
             
             hr(),
             
             plotOutput("plot4")
    ),
    
    # =====================
    # MODEL TAB
    # =====================
    
    tabPanel("Model",
             
             fluidRow(
               
               column(
                 6,
                 selectInput(
                   "model_x",
                   "Independent Variables:",
                   choices = predictor_cols,
                   selected = c("Age"),
                   multiple = TRUE
                 )
               ),
               
               column(
                 6,
                 selectInput(
                   "plot_var",
                   "Variable To Plot:",
                   choices = predictor_cols,
                   selected = "Age"
                 )
               )
             ),
             
             hr(),
             
             h3("Regression Equation"),
             verbatimTextOutput("equation"),
             
             hr(),
             
             plotOutput("model_plot"),
             
             hr(),
             
             h3("Model Statistics"),
             verbatimTextOutput("model_stats"),
             
             hr(),
             
             h3("Model Summary"),
             verbatimTextOutput("model_summary")
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output, session) {
  
  # ---------------------
  # SCATTER
  # ---------------------
  
  output$plot1 <- renderPlot({
    
    ggplot(
      df,
      aes(
        x = .data[[input$x_var_1]],
        y = .data[[input$y_var_1]]
      )
    ) +
      
      {
        if(input$color_var_1 != "None") {
          geom_point(
            aes(color = .data[[input$color_var_1]]),
            alpha = 0.6,
            size = 3
          )
        } else {
          geom_point(color = "orange", alpha = 0.6, size = 3)
        }
      } +
      
      theme_minimal()
  })
  
  # ---------------------
  # LINE
  # ---------------------
  
  output$plot2 <- renderPlot({
    
    if(input$group_var_2 != "None") {
      
      agg <- aggregate(
        df[[input$y_var_2]],
        by = list(
          x = df[[input$x_var_2]],
          g = df[[input$group_var_2]]
        ),
        FUN = mean
      )
      
      names(agg) <- c("x","group","y")
      
      ggplot(
        agg,
        aes(x = x, y = y, color = group, group = group)
      ) +
        geom_line() +
        geom_point() +
        theme_minimal()
      
    } else {
      
      agg <- aggregate(
        df[[input$y_var_2]],
        by = list(x = df[[input$x_var_2]]),
        FUN = mean
      )
      
      names(agg) <- c("x","y")
      
      ggplot(
        agg,
        aes(x = x, y = y)
      ) +
        geom_line(color = "orange") +
        geom_point(color = "navy") +
        theme_minimal()
    }
  })
  
  # ---------------------
  # HISTOGRAM
  # ---------------------
  
  output$plot3 <- renderPlot({
    
    p <- ggplot(
      df,
      aes(x = .data[[input$hist_var]])
    ) +
      geom_histogram(
        bins = input$bins,
        fill = "orange",
        color = "white"
      ) +
      theme_minimal()
    
    if(input$facet_var_3 != "None") {
      p <- p +
        facet_wrap(vars(.data[[input$facet_var_3]]))
    }
    
    p
  })
  
  # ---------------------
  # BOXPLOT
  # ---------------------
  
  output$plot4 <- renderPlot({
    
    ggplot(
      df,
      aes(
        x = as.factor(.data[[input$x_var_4]]),
        y = .data[[input$y_var_4]]
      )
    ) +
      geom_boxplot(fill = "orange", alpha = 0.7) +
      {
        if(input$show_points)
          geom_jitter(width = 0.2, alpha = 0.4)
      } +
      theme_minimal()
  })
  
  # =========================
  # REGRESSION MODEL
  # =========================
  
  reg_model <- reactive({
    
    req(length(input$model_x) > 0)
    
    lm(
      reformulate(
        input$model_x,
        response = "Absenteeism.time.in.hours"
      ),
      data = df
    )
  })
  
  # Equation
  output$equation <- renderPrint({
    
    model <- reg_model()
    
    coefs <- round(coef(model), 3)
    
    eq <- paste0(
      "Absenteeism.time.in.hours = ",
      coefs[1]
    )
    
    if(length(coefs) > 1){
      
      for(i in 2:length(coefs)){
        
        eq <- paste0(
          eq,
          " + ",
          coefs[i],
          "*",
          names(coefs)[i]
        )
      }
    }
    
    cat(eq)
  })
  
  # Regression plot
  output$model_plot <- renderPlot({
    
    ggplot(
      df,
      aes(
        x = .data[[input$plot_var]],
        y = Absenteeism.time.in.hours
      )
    ) +
      
      geom_point(
        color = "steelblue",
        alpha = 0.6,
        size = 3
      ) +
      
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "red"
      ) +
      
      theme_minimal() +
      
      labs(
        title = paste("Absenteeism vs", input$plot_var),
        x = input$plot_var,
        y = "Absenteeism time (hours)"
      )
  })
  
  # Stats
  output$model_stats <- renderPrint({
    
    model <- reg_model()
    
    summ <- summary(model)
    
    r2 <- round(summ$r.squared, 4)
    adj_r2 <- round(summ$adj.r.squared, 4)
    
    pval <- signif(
      pf(
        summ$fstatistic[1],
        summ$fstatistic[2],
        summ$fstatistic[3],
        lower.tail = FALSE
      ),
      4
    )
    
    cat("R Squared:", r2, "\n")
    cat("Adjusted R Squared:", adj_r2, "\n")
    cat("Overall Model P-value:", pval)
  })
  
  # Full summary
  output$model_summary <- renderPrint({
    
    summary(reg_model())
    
  })
}


#launch app
shinyApp(ui, server)


