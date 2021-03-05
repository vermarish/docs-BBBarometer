library(shiny)
library(tidyverse)

ui <- fluidPage(
  # App title
  titlePanel("Big Brother Barometer"),
  
  # Sidebar layout (contains I/O definitions)
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      
      p("Imagine typing a passcode onto your phone. With each input, the force of your thumb on the display causes two phenomena."),
      
      p("Phenomenon #1: the pressure sensor records a sudden change in pressure due to your thumb changing the volume of the phone."),
      
      p("Phenomenon #2: the gyroscope records a sudden change in rotation velocity due to your thumb pushing your phone in your hand."),
      
      p("The goal of this project is malicious: use the pressure/gyroscope sensors to guess a user's passcode. This project is a work in progress."),
      
      hr(),
      
      h4("Orange lines represent vertical input."), 

      h4("For a start, notice that each orange bar is immediately followed by a pressure spike, and accompanied by a gyroscope spike."),
      hr(),
      checkboxGroupInput(inputId = "categories",
                   label = "Categories:",
                   choices = c("Pressure"="p", "Gyroscope One"="g1", 
                                  "Gyroscope Two"="g2", "Gyroscope Three"="g3"),
                   selected = c("p")
      ), sliderInput(inputId = "range",
                     label = "Range:",
                     min = 0,
                     max = 1,
                     value = c(0.03, 0.15),
                     step = 0.01
      ), sliderInput(inputId = "fontsize",
                     label = "Font Size:",
                     min = 10,
                     max = 24,
                     value = 18,
                     step = 1
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      plotOutput(outputId = "timeSeries", height="75vh"), width=7
    )
  )
)


server <- function(input, output) {
  # This expression stores a histogram. 
  #      It is reactive to change in input.
  output$timeSeries <- renderPlot({
    y <- input$categories
    if (length(y) > 0) {
      plot <- scatterplot(data=tidbits, values=y, start=input$range[1], end=input$range[2])
    } else{
      plot <- scatterplot(data=tidbits, start=input$range[1], end=input$range[2])
    }
    #ggplotly(plot + theme(text = element_text(size=input$fontsize)))
    plot + theme(text = element_text(size=input$fontsize))
  })
}

# Get data
tidbits <- read_csv("data/data.csv")

# Calibrate the touch
uncalibrated_touch <- tidbits %>%
  filter(type == -27)

elapsedRealtimeNanosEnd <- tidbits %>% 
  filter(type == -2) %>% 
  select(time) %>% 
  slice(2) %>% 
  as.double()

uptimeMillisEnd <- tidbits %>% 
  filter(type == -1) %>% 
  select(time) %>% 
  slice(2) %>% 
  as.double() 

m <- 1e+6
b = elapsedRealtimeNanosEnd - m*uptimeMillisEnd

touch <- uncalibrated_touch %>% 
  mutate(time = m*time + b)

tidbits <- tidbits %>% 
  filter(type != -27) %>%
  union(touch)

# Scale pressure values so they fit on the same y-axis as gyrosocope data.
highest_g <- tidbits %>%
  filter(type == 4) %>%  # gyroscope 
  select(one, two, three) %>%
  max()

lowest_g <- tidbits %>%
  filter(type == 4) %>%
  select(one, two, three) %>%
  min()

amplitude <- max(abs(c(highest_g, lowest_g)))

# now min pressure maps to -amplitude
# and max pressure maps to +amplitude
# y = y1 + (y1-y0)/(x1-x0)(x-x1)
# where x is pressure, y is scaled pressure

pressure <- tidbits %>% filter(type == 6)

highest_p <- pressure %>%
  select(one) %>%
  max()

lowest_p <- pressure %>%
  select(one) %>%
  min()

m <- (amplitude+amplitude)/(highest_p-lowest_p)

pressure <- pressure %>%
  mutate(one = amplitude + m*(one - highest_p))

tidbits <- tidbits %>%
  filter(type != 6) %>%
  union(pressure)


### GRAPHING
### Includes variables first_touch, last_touch, range
### Includes functions trim(), scatterplot()
###
### The design is to pass a variable and a range into scatterplot().
### scatterplot() gets the appropriate data with trim(), which 
###    itself uses first_touch, last_touch, range. 
### scatterplot() then returns a graph for this data
first_touch = touch %>% 
  select(time) %>%
  min %>%
  as.double

last_touch = touch %>% 
  select(time) %>%
  max %>%
  as.double

range = last_touch - first_touch

## 0 <= start < end <= 1
trim <- function(data, start, end) {
  open = range*start + first_touch
  close = range*end + first_touch
  
  return(data %>%
           filter(time > open & time < close))
}


## Pre-conditions not enforced:
## value = {'one', 'two', 'three', 'one_smooth', 'two_smooth', 'three_smooth'}
## 0 <= start < end <= 1
scatterplot <- function(data, values=NULL, start, end) {   
  # Use start/end to find a proportion of the touch input. Then find all sensor data within.
  # (Decided because multiple sensors are used, but there is only one touch dataset)
  
  data <- data %>%
    trim(start, end) 
  
  graph <- ggplot()
  
  i = 1
  while (i <= length(values)) {
    value = values[i]
    if (value == 'p') {
      graph <- graph +
        geom_line(data=filter(data, type==6), 
                  aes(x=time, y=one),
                  colour="#003fec")
    } else if (value == 'g1') {
      graph <- graph +
        geom_line(data=filter(data, type==4),
                  aes(x=time, y=one),
                  color="#38d05d")
    } else if (value == 'g2') {
      graph <- graph +
        geom_line(data=filter(data, type==4),
                  aes(x=time, y=two),
                  color="#bc5090")
    } else if (value == 'g3') {
      graph <- graph +
        geom_line(data=filter(data, type==4),
                  aes(x=time, y=three),
                  color="#ff6361")
    }
    i = i + 1
  }
  
  graph <- graph + scale_y_continuous()
  
  # Add title and subtitle
  duration <- (range*(end-start) / 1e9 ) %>% round(2) %>% toString()
  #graph <- graph + ylab(deparse(substitute(data))) + ggtitle(paste(deparse(substitute(data)), value),
  #                                                          subtitle=paste("Data captured across", duration, "seconds."))
  graph <- graph + ggtitle("", subtitle=paste("Data captured across", duration, "seconds of touch input."))
  
  # Add vertical indication for touch input
  taps <- touch %>% trim(start, end)
  for (i in 1:as.integer(count(taps))) {
    t <- slice(taps, i)[2] %>% as.double()
    graph <- graph + geom_vline(xintercept = t, colour = "#ffa600", size=1)
  }
  
  # Scale x,y to the window
  open = range*start + first_touch
  close = range*end + first_touch
  graph <- graph + 
    xlim(open, close) + 
    ylim(-amplitude, amplitude)
  
  graph <- graph + 
    xlab("time (nanoseconds") +
    ylab("data value")
  
  return(graph)
}

shinyApp(ui=ui, server=server)