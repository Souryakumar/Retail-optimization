#loading shiny app
library(shiny)
#coding ui for project
ui <- fluidPage(theme = "btheme.css",pageWithSidebar(
  headerPanel(h3("Descriptive Analysis")),
  sidebarPanel(width=3,
    selectInput("fun",
                h6("Select functions(for content)"),
                list("Summary" = 1, 
                     "Structure" = 2, 
                     "Head" = 3,
                     "Tail"=4,
                     "Names"=5),selected = 1),
    
    selectInput ("p", "Select X-axis:(for 1st chart)",
                 list("Item_Visibility"='d1',"Item_MRP"='f1', 
                      "Outlet_Establishment_Year"='h1',
                      "Item_Outlet_Sales"='l1'),selected = 'd1'),
    sliderInput("bins",
                "Number of bins(for 1st chart):",
                min = 1,
                max = 50,
                value = 30),
  
    
    selectInput ("s", h6("Select X-axis:(for 2nd chart)"),
                 list("Item_Identifier"='a1', "Item_Weight"='b1', "Item_Fat_Content"='c1', 
                      "Item_Visibility"='d1',"Item_Type"='e1', "Item_MRP"='f1', 
                      "Outlet_Identifier"='g1', "Outlet_Establishment_Year"='h1',
                      "Outlet_Size"='i1', "Outlet_Location_Type"='j1', "Outlet_Type"='k1',
                      "Item_Outlet_Sales"='l1'),selected = 'd1'),
  
    
    selectInput ("k", h6("Select Y-axis:(for 2nd chart)"),
                 list("Item_Identifier"='a2', "Item_Weight"='b2', "Item_Fat_Content"='c2', 
                      "Item_Visibility"='d2',"Item_Type"='e2', "Item_MRP"='f2', 
                      "Outlet_Identifier"='g2', "Outlet_Establishment_Year"='h2',
                      "Outlet_Size"='i2', "Outlet_Location_Type"='j2', "Outlet_Type"='k2',
                      "Item_Outlet_Sales"='l2'),selected = 'j2'),
    
    
    selectInput("me",
                h6("Select measure"),
                list("Mean" = 1, 
                     "Median" = 2, 
                     "Max" = 3,
                     "Min"=4,
                     "Range"=5,
                     "Standard  Deviation"=6),selected = 1 ),
    
    selectInput("var",
                h6("Select variable(for measures)"),
                list("Item_Weight" = "Item_Weight", 
                     "Item_Outlet_Sales" = "Item_Outlet_Sales",
                     "Item_Fat_Content" = "Item_Fat_Content",
                     "Item_Visibility" = "Item_Visibility",
                     "Item_MRP" = "Item_MRP",
                     "Outlet_Size" = "Outlet_Size",
                     "Outlet_Location_Type" = "Outlet_Location_Type",
                     "Item_Outlet_Sales" = "Item_Outlet_Sales"),selected = "Item_weight" )),
  
  #making Main panel
  mainPanel(width=9,tabsetPanel(
    tabPanel(h4("Content"),verbatimTextOutput("cont")),
    tabPanel(h4("charts single"),plotOutput("distPlot1")),
    tabPanel(h4("charts double"),plotOutput("distPlot2")),
    tabPanel(h4("Measures"),verbatimTextOutput("meas")),
    tabPanel(h4("Predict data"),verbatimTextOutput("predata")),
    tabPanel(h4("predict Graph"),plotOutput("pred"))))
))

#server part of project
server <- function(input, output) {

  

 #for chart 1
  output$distPlot1 <- renderPlot({
    if(input$p=='a1') { i<-1 }
    if(input$p=='b1') { i<-2 }
    if(input$p=='c1') { i<-3 }
    if(input$p=='d1') { i<-4 }
    if(input$p=='e1') { i<-5 }
    if(input$p=='f1') { i<-6 }
    if(input$p=='g1') { i<-7 }
    if(input$p=='h1') { i<-8 }
    if(input$p=='i1') { i<-9 }
    if(input$p=='j1') { i<-10 }
    if(input$p=='k1') { i<-11 }
    if(input$p=='l1') { i<-12 }
    if(input$p=='m1') { i<-13 }
    
    #read data
    library(readr)
    train<-read.csv("C:/Users/sourya thakur/Desktop/sopra project/Retail Optimization/newretail/train.csv")
    x    <- train[, i]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'green', border = 'red')
    
  })
  
  # for chart 2
  library(readr)
  library(ggplot2)
  train<-read.csv("C:/Users/sourya thakur/Desktop/sopra project/Retail Optimization/newretail/train.csv")
      output$distPlot2 <- renderPlot({
      if(input$s=='a1') { i<-1 }
      if(input$s=='b1') { i<-2 }
      if(input$s=='c1') { i<-3 }
      if(input$s=='d1') { i<-4 }
      if(input$s=='e1') { i<-5 }
      if(input$s=='f1') { i<-6 }
      if(input$s=='g1') { i<-7 }
      if(input$s=='h1') { i<-8 }
      if(input$s=='i1') { i<-9 }
      if(input$s=='j1') { i<-10 }
      if(input$s=='k1') { i<-11 }
      if(input$s=='l1') { i<-12 }
      if(input$s=='m1') { i<-13 }
      
      if(input$k=='a2') { j<-1 }
      if(input$k=='b2') { j<-2 }
      if(input$k=='c2') { j<-3 }
      if(input$k=='d2') { j<-4 }
      if(input$k=='e2') { j<-5 }
      if(input$k=='f2') { j<-6 }
      if(input$k=='g2') { j<-7 }
      if(input$k=='h2') { j<-8 }
      if(input$k=='i2') { j<-9 }
      if(input$k=='j2') { j<-10 }
      if(input$k=='k2') { j<-11 }
      if(input$k=='l2') { j<-12 }
      if(input$k=='m2') { j<-13 }
      
      #read data
      library(readr)
      train<-read.csv("C:/Users/sourya thakur/Desktop/sopra project/Retail Optimization/newretail/train.csv")
      x<-train[,i]
      y<-train[,j]
      ggplot(train,aes(x=x,y=y))+geom_point()+geom_smooth(method="lm")
      
      
  })
      #for functions
      library(readr)
      train<-read.csv("C:/Users/sourya thakur/Desktop/sopra project/Retail Optimization/newretail/train.csv")
  
     output$cont <- renderPrint({
    if (input$fun == 1 ){
      print(summary(train))
    }
    else if (input$fun == 2 ){
      print(str(train))
    }
    else if (input$fun == 3 ){
      print(head(train))
    }
    else if (input$fun == 4 ){
      print(tail(train))
    }
    else {
      print(names(train))
    }
  })
  output$meas <- renderPrint({
    if (input$me == 1 ){
      print(mean(train[[input$var]],na.rm = TRUE))
    }
    else if (input$me == 2 ){
      print(median(train[[input$var]],na.rm = TRUE))
    }
    else if (input$me == 3 ){
      print(max(train[[input$var]],na.rm = TRUE))
    }
    else if (input$me == 4 ){
      print(min(train[[input$var]],na.rm = TRUE))
    }
    else if (input$me == 5 ){
      print(range(train[[input$var]],na.rm = TRUE))
    }
    else{
      print(sd(train[[input$var]],na.rm = TRUE))
    }
  })
  
  #for ploating prediction RESIDUAL VS LEVERAGE
  output$pred <- renderPlot({
    #read data
    library(readr)
    library(ggplot2)
    train<-read.csv("C:/Users/sourya thakur/Desktop/sopra project/Retail Optimization/newretail/train.csv")
    test<-read.csv("C:/Users/sourya thakur/Desktop/sopra project/Retail Optimization/newretail/test.csv")
    #create a new variable in test file 
    test$Item_Outlet_Sales <- 1
    
    #combine train and test data
    combi <- rbind(train, test)
    
    #impute missing value in Item_Weight
    combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
    
    #impute 0 in item_visibility
    combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),combi$Item_Visibility)
    
    #rename level in Outlet_Size
    levels(combi$Outlet_Size)[1] <- "Other"
    
    #rename levels of Item_Fat_Content
    library(plyr)
    combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" ="Regular"))
    combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
    
    #create a new column 2013 - Year
    combi$Year <- 2013 - combi$Outlet_Establishment_Year
    
    #drop variables not required in modeling
    library(dplyr)
    combi <- select(combi, -c(Item_Identifier, Outlet_Identifier))
    
    #divide data set
    new_train <- combi[1:nrow(train),]
    new_test <- combi[-(1:nrow(train)),]
    
    #linear regression
    linear_model <- lm(Item_Visibility~Item_Outlet_Sales , data = new_train)
    
    plot(linear_model,col="green")
  })
  
  # details of predection
  output$predata <- renderPrint({
    
    #read data
    library(readr)
    library(ggplot2)
    train<-read.csv("C:/Users/sourya thakur/Desktop/sopra project/Retail Optimization/newretail/train.csv")
    test<-read.csv("C:/Users/sourya thakur/Desktop/sopra project/Retail Optimization/newretail/test.csv")
    #create a new variable in test file 
    test$Item_Outlet_Sales <- 1
    
    #combine train and test data
    combi <- rbind(train, test)
    
    #impute missing value in Item_Weight
    combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
    
    #impute 0 in item_visibility
    combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),combi$Item_Visibility)
    
    #rename level in Outlet_Size
    levels(combi$Outlet_Size)[1] <- "Other"
    
    #rename levels of Item_Fat_Content
    library(plyr)
    combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" ="Regular"))
    combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
    
    #create a new column 2013 - Year
    combi$Year <- 2013 - combi$Outlet_Establishment_Year
    
    #drop variables not required in modeling
    library(dplyr)
    combi <- select(combi, -c(Item_Identifier, Outlet_Identifier))
    
    #divide data set
    new_train <- combi[1:nrow(train),]
    new_test <- combi[-(1:nrow(train)),]
    
    #linear regression
    linear_model <- lm(Item_Visibility~Item_Outlet_Sales , data = new_train)
    
    summary(linear_model)
    
  })
  
}

shinyApp(ui = ui, server = server)