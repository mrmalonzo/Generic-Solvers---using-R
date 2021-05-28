library(shiny) 
library(reshape2)
source("POLYNOMIAL REGRESSION.R") #import my polynomial regression r
source("QUADRATIC SPLINE.R") #import my quadratic spline r
source("SIMPLEX.R")

#use navbarpage for project when compiling them

ui <- navbarPage("CMSC 150 PROJECT", #my ui
    tabPanel(
     "Polynomial Regression", #display title
     
     titlePanel(h1("Polynomial Regression", align = "center")), #display title
      sidebarLayout( #my side bar lay out
        sidebarPanel(h2("Inputs:"), 
                     fileInput("tableofvalues",label="File to Solve", multiple = FALSE, accept=c(
                       "text/csv", #ask for the csv file
                       "text/comma-separated-values,text/plain", #only accept csv file
                       ".csv"), width=NULL, buttonLabel = "Browse", placeholder = "Please select a .csv file"), 
                     numericInput("degree", label="DEGREE", 1, min=0, max=4, width=NULL), #ask for the degree
                     numericInput("estimatedvalue", label="ESTIMATED VALUE", 1, min=NA, max=NA, width=NULL), #ask for the estimated value
        ),
        mainPanel( #main panel
          h3("Answer in accordance to the estimated value: ",), 
          h2(textOutput("Answer"), align = "center"), #print the answer according to the estimated value
          h3("Function built: ",),
          h2(textOutput("Function"), align= "center"), #print the function formed
        )
      )
    ),
    tabPanel(
      "Quadratic Spline",
      titlePanel(h1("Quadratic Spline", align = "center")), #display title
      sidebarLayout( #my side bar lay out
        sidebarPanel(h2("Inputs:"), 
                     fileInput("tableofvalues2",label="File to Solve", multiple = FALSE, accept=c(
                       "text/csv", #ask for the csv file
                       "text/comma-separated-values,text/plain", #only accept csv file
                       ".csv"), width=NULL, buttonLabel = "Browse", placeholder = "Please select a .csv file"), 
                     numericInput("estimatedvalue2", label="ESTIMATED VALUE", 1, min=NA, max=NA, width=NULL), #ask for the estimated value
        ),
        mainPanel( #main panel
          h3("Answer in accordance to the estimated value: ",), 
          h2(textOutput("Answer2"), align = "center"), #print the answer according to the estimated value
          h3("Function built in accordance to the interval of the given estimated value: ",),
          h2(textOutput("Function2"), align= "center"), #print the function formed
        )
      )
      
    ),
    tabPanel(
      "Simplex",
      titlePanel(h1("Simplex", align = "center"),), #display title
      # tabPanel( type="tabs",
        # tabPanel( "Simplex Input",
          h4("*Please maximize the screen to see the data more clearly", align = "center"),
          fluidRow(
            column(1,
                   h2("Plants" , align = "center"),
                   h2("|" , align = "center"),
                   h3("Denver:"),
                   h2("|" , align = "center"),
                   h3("Phoenix:"),
                   h2("|" , align = "center"),
                   h3("Dallas:"),
            ),
            column(1,
                   h3("Supply per Factory:"),
                   numericInput("supplyDenver", label="Supply of Denver", 310, min=0, max=1000, width=80),
                   numericInput("supplyPhoenix", label="Supply of Phoenix", 260, min=0, max=1000, width=80),
                   numericInput("supplyDallas", label="Supply of Dallas", 280, min=0, max=1000, width=80),
            ),
            column(1, offset=1,
              numericInput("demandSacramento", label="Demands by Sacramento", 180, min=0, max=1000, width=80),
              numericInput("costDenverSacramento", label="Shipping cost Sacramento", 10, min=0, max=1000, width=100),
              numericInput("costPhoenixSacramento", label="Shipping cost Sacramento", 6, min=0, max=1000, width=100),
              numericInput("costDallasSacramento", label="Shipping cost Sacramento", 3, min=0, max=1000, width=100),
            ),
            column(1, offset=1,
              numericInput("demandSaltlake", label="Demands by Salt Lake", 80, min=0, max=1000, width=80),
              numericInput("costDenverSaltlake", label="Shipping cost to Salt Lake", 8, min=0, max=1000, width=100),
              numericInput("costPhoenixSaltlake", label="Shipping cost to Salt Lake", 5, min=0, max=1000, width=100),
              numericInput("costDallasSaltlake", label="Shipping cost to Salt Lake", 4, min=0, max=1000, width=100),
            ),
            column(1, offset=1,
              numericInput("demandChicago", label="Demands by Chicago City", 200, min=0, max=1000, width=80),
              numericInput("costDenverChicago", label="Shipping cost to Chicago", 6, min=0, max=1000, width=100),
              numericInput("costPhoenixChicago", label="Shipping cost to Chicago", 4, min=0, max=1000, width=100),
              numericInput("costDallasChicago", label="Shipping cost to Chicago", 5, min=0, max=1000, width=100),
            ),
            column(1, offset=1,
              numericInput("demandAlbuqurque", label="Demands by Albuqurque", 160, min=0, max=1000, width=80),
              numericInput("costDenverAlbuqurque", label="Shipping cost to Albaqurque", 5, min=0, max=1000, width=100),
              numericInput("costPhoenixAlbuqurque", label="Shipping cost to Albuqurque", 3, min=0, max=1000, width=100),
              numericInput("costDallasAlbaqurque", label="Shipping cost to Albuqurque", 5, min=0, max=1000, width=100),
            ),
            column(1, offset=1,
              numericInput("demandNewyork", label="Demands by New York", 220, min=0, max=1000, width=80),
              numericInput("costDenverNewYork", label="Shipping cost to New York", 4, min=0, max=1000, width=100),
              numericInput("costPhoenixNewYork", label="Shipping cost to New York", 6, min=0, max=1000, width=100),
              numericInput("costDallasYork", label="Shipping cost to New York", 9, min=0, max=1000, width=100),
            ),
           
          ),
          h3("Objective Function:"),
          h3(textOutput("objectivefunction"), align="center"),
          h3("Constraints:"),
          h4(textOutput("constraint1"), align = "center"),
          h4(textOutput("constraint2"), align = "center"),
          h4(textOutput("constraint3"), align = "center"),
          h4(textOutput("constraint4"), align = "center"),
          h4(textOutput("constraint5"), align = "center"),
          h4(textOutput("constraint6"), align = "center"),
          h4(textOutput("constraint7"), align = "center"), 
          h4(textOutput("constraint8"), align = "center"),
          
          checkboxInput("showtable", "Show Tables", value = TRUE, width = NULL), #checkbox to see if the user wants to see the table
          conditionalPanel(condition = "input.showtable == 1", #if user wants to see the table
              h3("Initial Tableau:"),
              tableOutput('InitialTable')),
        # ),
        # tabPanel( "Simplex Output",
          h3("TABLEAU OF SIMPLEX:"),
          
          fluidRow(
            column( 1,
              numericInput("iteration", label="Iteration", 1, min=, max=, width=NULL),
            ),
            column( 1,
              h4("Max Iterations:" , inline=TRUE),
            ),column( 1,
              h4(textOutput("maxiter", inline=TRUE )
            ),
            
            
          ),
          conditionalPanel(condition = "input.showtable == 1", #if user wants to see the table
                           tableOutput('answers'), #print the table
                          ),
         
          
          fluidRow( #print the final answer
            column( 2,
                    h2("MINIMIZED COST:" , inline=TRUE),
            ),
            column( 2,
                      h2(textOutput("FinalAnswer", inline=TRUE )),
            ),
          ),
         
          ),
      
    )
)


server <- function(input, output){#server function
  output$Answer <- renderText({ #to render my answer
    myTable <- input$tableofvalues #get the table of values in my input
    
    if(is.null(myTable))  #if csv table given is a null
      return(NULL)
    data<-read.csv(myTable$datapath) #get the table in the csv
    
    mydegree <- input$degree #get the degree in the input
    myestimatedvalue <- input$estimatedvalue #get the estimated value in my inputs
    polyregression=PolynomialRegression(data,mydegree,myestimatedvalue) #access my polynomial regression function that returns the answer according to the estimate value and the function formed
    paste(polyregression[2]) #print the answer
  })
  output$Function <- renderText({ #to render my function
    myTable <- input$tableofvalues #almost the same as the output above
    
    if(is.null(myTable))  #if csv table given is a null
      return(NULL)
    data<-read.csv(myTable$datapath) #get the table in the csv
    
    mydegree <- input$degree
    myestimatedvalue <- input$estimatedvalue
    polyregression=PolynomialRegression(data,mydegree,myestimatedvalue)
    paste(polyregression[1]) #print the function solved
  })
  output$Answer2 <- renderText({ #to render my answer
    myTable2 <- input$tableofvalues2 #get the table of values in my input
    
    if(is.null(myTable2))  #if csv table given is a null
      return(NULL)
    data<-read.csv(myTable2$datapath) #get the table in the csv
    
    myestimatedvalue2 <- input$estimatedvalue2 #get the estimated value in my inputs
    quadspline=QuadraticSpline(data,myestimatedvalue2) #access my polynomial regression function that returns the answer according to the estimate value and the function formed
    paste(quadspline[2]) #print the answer
  })
  output$Function2 <- renderText({ #to render my answer
    myTable2 <- input$tableofvalues2 #get the table of values in my input
    
    if(is.null(myTable2))  #if csv table given is a null
      return(NULL)
    data<-read.csv(myTable2$datapath) #get the table in the csv
    
    myestimatedvalue2 <- input$estimatedvalue2 #get the estimated value in my inputs
    quadspline=QuadraticSpline(data,myestimatedvalue2) #access my polynomial regression function that returns the answer according to the estimate value and the function formed
    paste(quadspline[1]) #print the answer
  })
  
 
  
  
  output$InitialTable <- renderTable({
    initialtable = matrix(c(0), nrow=9, ncol=25, dimnames=list(c(),c("x1", "x2",  "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "z","solution" )));
    for(i in 1:15){ #put all the negative 1's in my table (because im using dual of a matrix and i multiplied my >= constraints by -1)
      for(j in 1:15){ #for 1st to 5th constraints
        if((i==j && i<=5) || i<=5 && (j==5+i || j==10+i)){
          initialtable[i,j]=-1
        }
      }
    }
    for(i in 1:9){
      for(j in 16:24){ #put all the slack variables iny my table
        if(j==15+i){
          initialtable[i,j]=1
        }
      }
    }
    for(i in 1:15){  # put all the 1's for the 6th, 7th and 8th constraints
      if(i<=5){
        initialtable[6,i]=1; 
      }else if(i<=10){
        initialtable[7,i]=1;
      }else{
        initialtable[8,i]=1;
      }
    }
    #finally put all the reactive elements in the table. Note that I will multiply some values by negative 1 because im using dual simplex
    initialtable[1,25]=(input$demandSacramento)*-1; #put all the demans in the solutions and multiply it by negative 1 because its >= and we want it to be <=
    initialtable[2,25]=(input$demandSaltlake)*-1;
    initialtable[3,25]=(input$demandChicago)*-1;
    initialtable[4,25]=(input$demandAlbuqurque)*-1;
    initialtable[5,25]=(input$demandNewyork)*-1;
    
    initialtable[6,25]=input$supplyDenver; # put the reactive values supply per factory in the table
    initialtable[7,25]=input$supplyPhoenix;
    initialtable[8,25]=input$supplyDallas;
    
    #finally put the cost of shipping in the table and multiply it by 1 cause it is used in the objective function
    initialtable[9,1] = (input$costDenverSacramento)*-1; #for the delivery cost from denver to all the cities
    initialtable[9,2] = (input$costDenverSaltlake)*-1;
    initialtable[9,3] = (input$costDenverChicago)*-1;
    initialtable[9,4] = (input$costDenverAlbuqurque)*-1;
    initialtable[9,5] = (input$costDenverNewYork)*-1;
    
    initialtable[9,6] = (input$costPhoenixSacramento)*-1; #for the delivery cost from Phoenix to all the cities
    initialtable[9,7] = (input$costPhoenixSaltlake)*-1;
    initialtable[9,8] = (input$costPhoenixChicago)*-1;
    initialtable[9,9] = (input$costPhoenixAlbuqurque)*-1;
    initialtable[9,10] = (input$costPhoenixNewYork)*-1;
    
    initialtable[9,11] = (input$costDallasSacramento)*-1; #for the delivery cost from Dallas to all the cities
    initialtable[9,12] = (input$costDallasSaltlake)*-1;
    initialtable[9,13] = (input$costDallasChicago)*-1;
    initialtable[9,14] = (input$costDallasAlbaqurque)*-1;
    initialtable[9,15] = (input$costDallasYork)*-1;
    
    # Simplex(initialtable);
  
    initialtable;
  })
  output$objectivefunction <- renderText({
    eq<-paste("Z =  ", sep=""); #paste and concatinate to create a function
    eq<-paste(eq, input$costDenverSacramento, "x1 ",sep=""); #paste all the reactive cost values
    eq<-paste(eq, "+ ",input$costDenverSaltlake, "x2 ",sep="");
    eq<-paste(eq, "+ ",input$costDenverChicago, "x3 ",sep="");
    eq<-paste(eq, "+ ",input$costDenverAlbuqurque, "x4 ",sep="");
    eq<-paste(eq, "+ ",input$costDenverNewYork, "x5 ",sep="");
    eq<-paste(eq, "+ ",input$costPhoenixSacramento, "x6 ",sep="");
    eq<-paste(eq, "+ ",input$costPhoenixSaltlake, "x7 ",sep="");
    eq<-paste(eq, "+ ",input$costPhoenixChicago, "x8 ",sep="");
    eq<-paste(eq, "+ ",input$costPhoenixAlbuqurque, "x9 ",sep="");
    eq<-paste(eq, "+ ",input$costPhoenixNewYork, "x10 ",sep="");
    eq<-paste(eq, "+ ",input$costDallasSacramento, "x11 ",sep="");
    eq<-paste(eq, "+ ",input$costDallasSaltlake, "x12 ",sep="");
    eq<-paste(eq, "+ ",input$costDallasChicago, "x13 ",sep="");
    eq<-paste(eq, "+ ",input$costDallasAlbaqurque, "x14 ",sep="");
    eq<-paste(eq, "+ ",input$costDallasYork, "x15 ",sep="");

    paste(eq);   
  })
  
  output$constraint1 <- renderText({ #for the first constraint
    eq<-paste("x1 + x6 + x11 >=  ", sep=""); #paste the default and the reactice ddemand value
    eq<-paste(eq, input$demandSacramento, sep="");
  })
  output$constraint2 <- renderText({ #repeat for all the constraints
    eq<-paste("x2 + x7 + x12 >=  ", sep=""); #paste the default and the reactice ddemand value
    eq<-paste(eq, input$demandSaltlake, sep="");
  })
  output$constraint3 <- renderText({
    eq<-paste("x3 + x8 + x13 >=  ", sep=""); #paste the default and the reactice ddemand value
    eq<-paste(eq, input$demandChicago, sep="");
  })
  output$constraint4 <- renderText({
    eq<-paste("x4 + x9 + x14 >=  ", sep=""); #paste the default and the reactice ddemand value
    eq<-paste(eq, input$demandAlbuqurque, sep="");
  })
  output$constraint5 <- renderText({
    eq<-paste("x5 + x10 + x15 >=  ", sep=""); #paste the default and the reactice ddemand value
    eq<-paste(eq, input$demandNewyork, sep="");
  })
  output$constraint6 <- renderText({
    eq<-paste("x1 + x2 + x3 + x4 + x5 <=  ", sep=""); #paste the default and the reactice ddemand value
    eq<-paste(eq, input$supplyDenver, sep="");
  })
  output$constraint7 <- renderText({
    eq<-paste("x6 + x7 + x8 + x9 + x10 <=  ", sep=""); #paste the default and the reactice ddemand value
    eq<-paste(eq, input$supplyPhoenix, sep="");
  })
  output$constraint8 <- renderText({
    eq<-paste("x11 + x12 + x13 + x14 + x15 <=  ", sep=""); #paste the default and the reactice ddemand value
    eq<-paste(eq, input$supplyDallas, sep="");
  })
  
  output$maxiter <- renderText({ #for the final answer
    initialtable = matrix(c(0), nrow=9, ncol=25, dimnames=list(c(),c("x1", "x2",  "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "z","solution" )));
    for(i in 1:15){ #put all the negative 1's in my table (because im using dual of a matrix and i multiplied my >= constraints by -1)
      for(j in 1:15){ #for 1st to 5th constraints
        if((i==j && i<=5) || i<=5 && (j==5+i || j==10+i)){
          initialtable[i,j]=-1
        }
      }
    }
    for(i in 1:9){
      for(j in 16:24){ #put all the slack variables iny my table
        if(j==15+i){
          initialtable[i,j]=1
        }
      }
    }
    for(i in 1:15){  # put all the 1's for the 6th, 7th and 8th constraints
      if(i<=5){
        initialtable[6,i]=1; 
      }else if(i<=10){
        initialtable[7,i]=1;
      }else{
        initialtable[8,i]=1;
      }
    }
    #finally put all the reactive elements in the table. Note that I will multiply some values by negative 1 because im using dual simplex
    initialtable[1,25]=(input$demandSacramento)*-1; #put all the demans in the solutions and multiply it by negative 1 because its >= and we want it to be <=
    initialtable[2,25]=(input$demandSaltlake)*-1;
    initialtable[3,25]=(input$demandChicago)*-1;
    initialtable[4,25]=(input$demandAlbuqurque)*-1;
    initialtable[5,25]=(input$demandNewyork)*-1;
    
    initialtable[6,25]=input$supplyDenver; # put the reactive values supply per factory in the table
    initialtable[7,25]=input$supplyPhoenix;
    initialtable[8,25]=input$supplyDallas;
    
    #finally put the cost of shipping in the table and multiply it by 1 cause it is used in the objective function
    initialtable[9,1] = (input$costDenverSacramento)*-1; #for the delivery cost from denver to all the cities
    initialtable[9,2] = (input$costDenverSaltlake)*-1;
    initialtable[9,3] = (input$costDenverChicago)*-1;
    initialtable[9,4] = (input$costDenverAlbuqurque)*-1;
    initialtable[9,5] = (input$costDenverNewYork)*-1;
    
    initialtable[9,6] = (input$costPhoenixSacramento)*-1; #for the delivery cost from Phoenix to all the cities
    initialtable[9,7] = (input$costPhoenixSaltlake)*-1;
    initialtable[9,8] = (input$costPhoenixChicago)*-1;
    initialtable[9,9] = (input$costPhoenixAlbuqurque)*-1;
    initialtable[9,10] = (input$costPhoenixNewYork)*-1;
    
    initialtable[9,11] = (input$costDallasSacramento)*-1; #for the delivery cost from Dallas to all the cities
    initialtable[9,12] = (input$costDallasSaltlake)*-1;
    initialtable[9,13] = (input$costDallasChicago)*-1;
    initialtable[9,14] = (input$costDallasAlbaqurque)*-1;
    initialtable[9,15] = (input$costDallasYork)*-1;
    
    Listoftables=Simplex(initialtable);
    
    paste(Listoftables[length(Listoftables)]); #tell the user the max iterations of the table which is returned as the last element of the list
  })
  output$answers <- renderTable({ #for the final answer
    initialtable = matrix(c(0), nrow=9, ncol=25, dimnames=list(c(),c("x1", "x2",  "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "z","solution" )));
    for(i in 1:15){ #put all the negative 1's in my table (because im using dual of a matrix and i multiplied my >= constraints by -1)
      for(j in 1:15){ #for 1st to 5th constraints
        if((i==j && i<=5) || i<=5 && (j==5+i || j==10+i)){
          initialtable[i,j]=-1
        }
      }
    }
    for(i in 1:9){
      for(j in 16:24){ #put all the slack variables iny my table
        if(j==15+i){
          initialtable[i,j]=1
        }
      }
    }
    for(i in 1:15){  # put all the 1's for the 6th, 7th and 8th constraints
      if(i<=5){
        initialtable[6,i]=1; 
      }else if(i<=10){
        initialtable[7,i]=1;
      }else{
        initialtable[8,i]=1;
      }
    }
    #finally put all the reactive elements in the table. Note that I will multiply some values by negative 1 because im using dual simplex
    initialtable[1,25]=(input$demandSacramento)*-1; #put all the demans in the solutions and multiply it by negative 1 because its >= and we want it to be <=
    initialtable[2,25]=(input$demandSaltlake)*-1;
    initialtable[3,25]=(input$demandChicago)*-1;
    initialtable[4,25]=(input$demandAlbuqurque)*-1;
    initialtable[5,25]=(input$demandNewyork)*-1;
    
    initialtable[6,25]=input$supplyDenver; # put the reactive values supply per factory in the table
    initialtable[7,25]=input$supplyPhoenix;
    initialtable[8,25]=input$supplyDallas;
    
    #finally put the cost of shipping in the table and multiply it by 1 cause it is used in the objective function
    initialtable[9,1] = (input$costDenverSacramento)*-1; #for the delivery cost from denver to all the cities
    initialtable[9,2] = (input$costDenverSaltlake)*-1;
    initialtable[9,3] = (input$costDenverChicago)*-1;
    initialtable[9,4] = (input$costDenverAlbuqurque)*-1;
    initialtable[9,5] = (input$costDenverNewYork)*-1;
    
    initialtable[9,6] = (input$costPhoenixSacramento)*-1; #for the delivery cost from Phoenix to all the cities
    initialtable[9,7] = (input$costPhoenixSaltlake)*-1;
    initialtable[9,8] = (input$costPhoenixChicago)*-1;
    initialtable[9,9] = (input$costPhoenixAlbuqurque)*-1;
    initialtable[9,10] = (input$costPhoenixNewYork)*-1;
    
    initialtable[9,11] = (input$costDallasSacramento)*-1; #for the delivery cost from Dallas to all the cities
    initialtable[9,12] = (input$costDallasSaltlake)*-1;
    initialtable[9,13] = (input$costDallasChicago)*-1;
    initialtable[9,14] = (input$costDallasAlbaqurque)*-1;
    initialtable[9,15] = (input$costDallasYork)*-1;
    
    Listoftables=Simplex(initialtable);
    
    Listoftables[[input$iteration]]; #get the table that the user asks for
  })
  
  output$FinalAnswer <- renderText({ #for the final answer
    initialtable = matrix(c(0), nrow=9, ncol=25, dimnames=list(c(),c("x1", "x2",  "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "z","solution" )));
    for(i in 1:15){ #put all the negative 1's in my table (because im using dual of a matrix and i multiplied my >= constraints by -1)
      for(j in 1:15){ #for 1st to 5th constraints
        if((i==j && i<=5) || i<=5 && (j==5+i || j==10+i)){
          initialtable[i,j]=-1
        }
      }
    }
    for(i in 1:9){
      for(j in 16:24){ #put all the slack variables iny my table
        if(j==15+i){
          initialtable[i,j]=1
        }
      }
    }
    for(i in 1:15){  # put all the 1's for the 6th, 7th and 8th constraints
      if(i<=5){
        initialtable[6,i]=1; 
      }else if(i<=10){
        initialtable[7,i]=1;
      }else{
        initialtable[8,i]=1;
      }
    }
    #finally put all the reactive elements in the table. Note that I will multiply some values by negative 1 because im using dual simplex
    initialtable[1,25]=(input$demandSacramento)*-1; #put all the demans in the solutions and multiply it by negative 1 because its >= and we want it to be <=
    initialtable[2,25]=(input$demandSaltlake)*-1;
    initialtable[3,25]=(input$demandChicago)*-1;
    initialtable[4,25]=(input$demandAlbuqurque)*-1;
    initialtable[5,25]=(input$demandNewyork)*-1;
    
    initialtable[6,25]=input$supplyDenver; # put the reactive values supply per factory in the table
    initialtable[7,25]=input$supplyPhoenix;
    initialtable[8,25]=input$supplyDallas;
    
    #finally put the cost of shipping in the table and multiply it by 1 cause it is used in the objective function
    initialtable[9,1] = (input$costDenverSacramento)*-1; #for the delivery cost from denver to all the cities
    initialtable[9,2] = (input$costDenverSaltlake)*-1;
    initialtable[9,3] = (input$costDenverChicago)*-1;
    initialtable[9,4] = (input$costDenverAlbuqurque)*-1;
    initialtable[9,5] = (input$costDenverNewYork)*-1;
    
    initialtable[9,6] = (input$costPhoenixSacramento)*-1; #for the delivery cost from Phoenix to all the cities
    initialtable[9,7] = (input$costPhoenixSaltlake)*-1;
    initialtable[9,8] = (input$costPhoenixChicago)*-1;
    initialtable[9,9] = (input$costPhoenixAlbuqurque)*-1;
    initialtable[9,10] = (input$costPhoenixNewYork)*-1;
    
    initialtable[9,11] = (input$costDallasSacramento)*-1; #for the delivery cost from Dallas to all the cities
    initialtable[9,12] = (input$costDallasSaltlake)*-1;
    initialtable[9,13] = (input$costDallasChicago)*-1;
    initialtable[9,14] = (input$costDallasAlbaqurque)*-1;
    initialtable[9,15] = (input$costDallasYork)*-1;
    
    Listoftables=Simplex(initialtable);
    
    Listoftables[[length(Listoftables)-1]][9,25]; #get the final minimized cost from the final table
  })
  
}

shinyApp(ui = ui , server=server)