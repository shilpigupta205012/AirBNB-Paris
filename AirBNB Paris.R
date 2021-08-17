library(shiny)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(gsubfn)

# Data Preparation Steps

load("AirBnB.Rdata")
L <- as.data.frame(L)
glimpse(L)
L <- subset(L,select= -c(listing_url,scrape_id,last_scraped,name,summary,space,description,neighborhood_overview,
                         notes,transit,access,interaction,thumbnail_url,medium_url,picture_url,xl_picture_url,
                         host_url,host_about,host_response_time,host_is_superhost,host_thumbnail_url,
                         host_picture_url,host_verifications,host_has_profile_pic,neighbourhood_group_cleansed,
                         city,market,country_code,country,is_location_exact,calendar_updated,has_availability,
                         calendar_last_scraped,first_review,last_review,requires_license,license,
                         instant_bookable,require_guest_profile_picture,require_guest_phone_verification,
                         square_feet,review_scores_accuracy,review_scores_cleanliness,review_scores_checkin,
                         review_scores_communication,review_scores_location,review_scores_value
))
glimpse(L)

L %>% distinct(zipcode)
L[L == "75 018"] <- "75018"
L[L == "750109"] <- "75019"
L[L == "700014"] <- "75014"
L[L == "75014\n75014"] <- "75014"
L[L == "75011 PARIS"] <- "75011"
L[L == "7517"] <- "75017"
L[L == "750016"] <- "75016"
L[L == "Paris 75004"] <- "75004"
L[L == "75019\n75019"] <- "75019"
L[L == "7009"] <- "75009"
L[L == "78008"] <- "75008"

Zipcode1 <- c(75001:75019)
L <- L %>% filter(zipcode == Zipcode1)

glimpse(L)
L <- L %>% mutate(arrondissement = case_when(
  L$zipcode == 75001 ~ "1",
  L$zipcode == 75002 ~ "2",
  L$zipcode == 75003 ~ "3",
  L$zipcode == 75004 ~ "4",
  L$zipcode == 75005 ~ "5",
  L$zipcode == 75006 ~ "6",
  L$zipcode == 75007 ~ "7",
  L$zipcode == 75008 ~ "8",
  L$zipcode == 75009 ~ "9",
  L$zipcode == 75010 ~ "10",
  L$zipcode == 75011 ~ "11",
  L$zipcode == 75012 ~ "12",
  L$zipcode == 75013 ~ "13",
  L$zipcode == 75014 ~ "14",
  L$zipcode == 75015 ~ "15",
  L$zipcode == 75016 ~ "16",
  L$zipcode == 75017 ~ "17",
  L$zipcode == 75018 ~ "18",
  L$zipcode == 75019 ~ "19",
)
)
attach(L)
L2 <- select(L,room_type,accommodates,bathrooms,bedrooms,beds,bed_type,price)

L2$room_type <- as.factor(L2$room_type)
L2$bed_type <- as.factor(L2$bed_type)
L2$bathrooms <- as.integer(L2$bathrooms)
L2$price = as.numeric(gsub("\\$", "", L2$price))

L5 <- select(L,price,host_neighbourhood,zipcode,arrondissement)
L5$price = as.numeric(gsub("\\$", "", L5$price))
L5$host_neighbourhood = as.factor(L5$host_neighbourhood)
L5  # Renting price for all city quarters
glimpse(L5)

#Renting price per city quarter(arrondissement)
L6 = L5 %>% group_by(arrondissement) %>% summarise(mean_price = mean(price))
L6

L3 <- select(L,host_id, host_listings_count,host_total_listings_count,calculated_host_listings_count)
L3 <- as.data.frame(L3)
glimpse(L3)

L4 <- L3 %>% distinct(host_id, .keep_all = TRUE)
glimpse(L4)

p_hist <- ggplot(L2, aes(x=price)) +
  geom_histogram(color="black", fill="white")
p_hist

L7 <- select(L,host_neighbourhood,number_of_reviews,arrondissement)
glimpse(L7)

L8 = L7 %>% group_by(arrondissement) %>% summarise(Visit_frequency = sum(number_of_reviews))
L8 %>% mutate(Visit_frequency = sum(number_of_reviews))
glimpse(L8)

ui <- fluidPage(
  
  # App title ----
  titlePanel("AirBnB Paris Data Visualization"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                  selected = "Blue",multiple = F),
      
      
      selectInput(inputId = "arrondissements", label = "Chose arrondissement", choices = L6$arrondissement,
                  selected = "1",multiple = F ),
      
      radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
      
      
      sliderInput(inputId = "bins1xz",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      tabsetPanel(type = "tabs",
                  tabPanel("Relationship between prices and apartment features", plotOutput("Plot1")),
                  tabPanel("Renting price per city quarter (arrondissements)", dataTableOutput("plot2")),
                  tabPanel("Number of apartments per owner", plotOutput("Plot3")),
                  tabPanel("Visit frequency of the different quarters according to time", dataTableOutput("plot4"))
      )
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output){
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$Plot1 <- renderPlot({
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    
    p2 <- L2 %>% ggplot()+
      geom_histogram(aes(x=price),bins = input$bins1xz, col=input$border1, fill=sColor)
    
    p2 <- p2 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Apartment price($)",y="Count",title=paste("Histogram of Price for AirBnB Paris apartments",sep = " "))
    
    p2
  })
  
  output$plot2 <- renderDataTable(L6)
  
  
  output$Plot3 <- renderPlot({
    
    p3 <- L4 %>% ggplot()+
      geom_bar(aes(x=calculated_host_listings_count), col=input$border1)
    
    p3 <- p3 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="host_listings_count",y="Count",title=paste("Number of apartments per owner",sep = " "))
    
    p3
  })
  
  output$plot4 <- renderDataTable(L8)
  
}

shinyApp(ui = ui, server = server)