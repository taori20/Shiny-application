## app.R ##
library(shinydashboard)
library(ggplot2)
library(MASS)  
require(tigerstats)
library(ROCR)
ui <- dashboardPage(
  dashboardHeader(title = "Statics dashboard"),
  dashboardSidebar(
    sidebarMenu(
      # adding menus 
      menuItem("Probability Models", tabName = "dashboard", icon = icon("dashboard"),startExpanded = TRUE,
               ## adding sub-menus for models
               menuSubItem("Discrete Model",tabName = "discrete"),
               menuSubItem("Continuous Model",tabName = "continuous")),
      ## adding 2nd menu 
      menuItem("ter", tabName = "widgets", icon = icon("th"),startExpanded = TRUE,
               ## adding sub-menus for models
               menuSubItem("Population Mean",tabName = "popmean"),
               menuSubItem("T-test",tabName = "ttest"),
               menuSubItem("Population Portion",tabName = "poportion"),
               menuSubItem("Varience",tabName = "var")),
      menuItem("Regression Models", tabName = "regression", icon = icon("th"),startExpanded = TRUE,
               ## adding sub-menus for models
               menuSubItem("Linear Model",tabName = "linear"),
               menuSubItem("Logistic Regression Model set",tabName = "logistic"),
               menuSubItem("K-NN Model",tabName = "knn"))
      
      
    )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems( 
      tabItem(
        tabName = "discrete",
        fluidRow(
          sidebarPanel(  
            selectInput("model", "Select Distribution  model",
                        c("poission" ="1"  ,"Binomial" = "2" , "Hypergeometric" = "3", 
                          "Geometric" = "4")),
            
            sliderInput("s", "sample size" ,min=0, max=1000, value = 10),
            conditionalPanel(
              "input.model == '3'",
              numericInput("nsd", "No of samples drawn urn" , value = 1),
              numericInput("ns", "No of samples in Container" , value = 1),
              numericInput("nsd2", "No of samples from urn 2" , value = 1),
              numericInput("ns2", "Sample in 2nd container" , value = 1)
            ),
            
            ## geometric model
            conditionalPanel(
              "input.model == '4'",
              numericInput("np", "Probability " , value = .1),
              numericInput("nt", "Fidn probability for Nth trail" , value = 1)
            ),
            
            
            conditionalPanel(
              "input.model == '1'",
              
              numericInput("lam", "parameter lambda in Poisson" , value = 1)),
            
            conditionalPanel(
              "input.model != '3'",
              numericInput("end", "ending value for x" , value = 10), 
              numericInput("j", "random value" , value = 1)),
            
            conditionalPanel(
              "input.model == '2'",
              
              numericInput("n", "n in Binomial" , value = 10),
              numericInput("p", "p in Binomial" , value = 0.1)),
            radioButtons("format","Download Format" ,choices = list("Pdf","png"))
            
          ),
          
          
          
          tabBox(  title = "Visualization for Probablility models",
                   # The id lets us use input$tabset1 on the server to find the current tab
                   id = "tabset1", height = "1150px",
                   
                   
                   tabPanel("Plot",
                            conditionalPanel(
                              "input.model == '1'",
                              plotOutput("plot_pois") ),
                            conditionalPanel(
                              "input.model == '2'",
                              plotOutput("plot") ),
                            
                            conditionalPanel(
                              "input.model == '3'",
                              plotOutput("plot_hyper") )
                            
                            
                   )
                   ,
                   tabPanel("Data", "Tab content 2")
          )
        )
        
      ),
      # menu tab 
      tabItem(
        tabName = "continuous",
        fluidRow(
          sidebarPanel(  
            selectInput("cmodel", "Select Continious  model",
                        c("Uniform" ="1"  ,"normal" = "2" , "exponential" = "3", 
                          "gamma" = "4", "Chi-squared" = "5" )),
            sliderInput("s", "sample size" ,min=0, max=1000, value = 10),
            
            ## Uniform model
            conditionalPanel(
              "input.cmodel == '1'",
              numericInput("a", "Starting  interval parameter" , value =1),
              numericInput("b", "Expected interval parameter" , value =1)
              
            ),
            ## Exponential 
            conditionalPanel(    
              condition = "input.cmodel == '3'",
              numericInput("lam", "parameter lambda in exponential" , value = 1) ,
              numericInput("j2", "j in exponential" , value = 0)
            ),
            ## Normal 
            conditionalPanel(
              condition = "input.cmodel == 'normal'",
              numericInput("mu", "parameter mu in Normal" , value = 0), 
              numericInput("sigma", "parameter sigma in Normal" , value = 1),
              numericInput("j1", "j in Normal" , value = 0)
            ),
            
            numericInput("i", "support" , value = 2),
            
            
            conditionalPanel(
              condition = "input.conmodel == 'uniform'",
              numericInput("a", "parameter a in Normal" , value = -2), 
              numericInput("b", "parameter b in Normal" , value = 0.8)
            )
          ),
          
          tabBox(title = "Visualization for Continious Models" ,id = "T2" , height = "1150px",
                 tabPanel("Plot", 
                          
                          
                          plotOutput("histogram") 
                          
                 ))
        )   ),
      
      tabItem(tabName = "var",
              fluidRow(
                navbarPage(strong("Varience") ,inverse = TRUE,
                           
                           tabPanel("Concept"
                                    
                           ),
                           tabPanel("Chi-Square",
                                    
                                    sidebarPanel(
                                      #  checkboxInput("dataset","Upload or use R dataset",value = TRUE),
                                      
                                      
                                      #  selectInput("dataset" , label = "Select dataset for Test", choices = c("mtcars", "iris")),
                                      br(),
                                      uiOutput("vx1"),
                                      uiOutput("vy1"),
                                      
                                      ## Selected data 
                                      tableOutput("selected_xdata"),
                                      tableOutput("selected_ydata"),
                                      #   table("selected_xdata","selected_ydata"),
                                      
                                      
                                      fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
                                      helpText("Default max. file size is 5MB"),
                                      br(),
                                      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';'), selected = ',')
                                    )
                                    ,
                                    tabBox( tabsetPanel(   
                                      tabPanel("Data",     
                                               tableOutput("table")   ),
                                      tabPanel("Results",
                                               verbatimTextOutput("test_result1") ),
                                      tabPanel("Graph")
                                    )
                                    
                                    
                                    )
                                    
                           ),
                           tabPanel("ANOVA")
                )
                
                
              )
              
      ),
      
      
      tabItem(tabName = "popmean",
              fluidRow(
                navbarPage(strong("popmean") ,inverse = TRUE,
                           
                           tabPanel("Z-table",
                                    
                                    sidebarPanel(
                                      tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                                                 tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
                                      ),
                                      numericInput("mu",label = HTML("$$ \\mu $$"), value = 10,min = 5,step = 1),
                                      br(),
                                      numericInput("xbar",HTML("$$ x&#772 $$"), value =12,min = 5,step = 1),
                                      numericInput("mua",label = HTML("$$ \\mu_1 $$"), value = 12,min = 5,step = 1),
                                      numericInput("sd","standard Devivation", value = 20,min = 5,step = 1),
                                      sliderInput("s", "sample size" ,min=0, max=1000, value = 10),
                                      radioButtons("Value","Tail Type",c("Left Tail" = "lesser","Right Tail"  = "greater","Two-Tailed" = "two.sided")),
                                      sliderInput("Alpha", "Significance Level" ,min=0, max=0.25, value = 0.5)
                                    )    ,
                                    tabBox(tabsetPanel(
                                      tabPanel("Result",
                                               fluidRow(
                                                 verbatimTextOutput("ttest"),
                                                 plotOutput("plot_out") 
                                                 
                                               ) 
                                      ),
                                      tabPanel("Z-table",
                                               plotOutput("plot_out1"))
                                    ))
                           )
                )
                
              )
      ),
      
     
      
      ## Regression Code
      tabItem(tabName = "logistic",
              sidebarPanel(
                selectInput("source","Select Your Source",choices = c("In-built" = "internal","Upload File" = "external")),
                conditionalPanel("input.source == 'internal'",
                                 selectInput("data_inbuilt" , label = "Select dataset for Test", choices = c("mtcars", "iris")),
                                 uiOutput("column_names"),
                                 uiOutput("response_variable"),
                                 sliderInput("split", "Split dataset" ,min=0.2, max=0.8, value = 0.8)
                ),
                conditionalPanel("input.source == 'external'",
                                 fileInput("file_reg","Upload your CSV",multiple = FALSE),
                                 uiOutput("column_names_upd"),
                                 uiOutput("response_variable_upd"),
                                 sliderInput("split", "Split dataset" ,min=0.2, max=0.8, value = 0.8),
                                 tags$hr(),
                                 h5(helpText("Select the read.table parameters below")),
                                 checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                 checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                 radioButtons(inputId = 'sep', label = 'Separator', 
                                              choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                )
                
                
              ),
              mainPanel(
                # textOutput("response_column")  
                tabsetPanel(   
                  tabPanel("Data",   
                           conditionalPanel("input.source == 'external'",
                                            tableOutput("tb1")
                           ),
                           conditionalPanel("input.source == 'internal'",
                                            tableOutput("regression_data") 
                           )
                  ),
                  tabPanel("Result" ,   
                           verbatimTextOutput("response_variable_data")  ) ,
                  tabPanel("test_result" ,   
                           tableOutput("test_table"
                           ) ,actionButton("show", "Check Accuracy Percentage") ) ,
                  tabPanel("Plot" ,   
                           plotOutput("reg_plot") 
                  )    
                  
                )
                
              )
      )
    )
    
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  output$plot <- renderPlot({
    
    par(mfrow=c(3,2))
    
    output$value <- renderPrint({ input$Model })
    # Binomial
    D1=rgeom(input$s, input$p)
    count=table(D1)
    barplot(count)
    x2=1:input$end
    y2=dgeom(x2,input$p)
    plot(x2,y2,type='b')
    
    #poisson
    D=rpois(input$s,input$lam)
    count=table(D)
    barplot(count, col='blue')
    x=0:input$end
    y=dpois(x,input$lam)
    plot(x,y,type='b')
    
    # Binomial
    D1=rbinom(input$s,input$n, input$p)
    count=table(D1)
    pie(count)
    x1=0:input$end
    y1=dbinom(x,input$n, input$p)
    plot(x1,y1,type='b')
    
  })
  
  output$plot_pois <- renderPlot({
    
    
    #poisson
    D=rpois(input$s,input$lam)
    count=table(D)
    barplot(count, col='blue')
    x=0:input$end
    y=dpois(x,input$lam)
    plot(x,y,type='b')
    
    
  })
  
  
  output$plot_hyper <- renderPlot({
    # Hypergeometric model
    H1=1-phyper(input$nsd,input$ns, input$nsd2, input$ns2)
    count=table(H1)
    pie(count)
    x1=0:input$end
    y1=dhyper(x,input$n, input$p)
    plot(x1,y1,type='b')
  })
  
  output$histogram <- renderPlot({
    
    # normal 
    if (input$cmodel == '2') {
      par(mfrow=c(1,2)) 
      x=seq(-input$i,input$i,0.01) 
      plot(x,dnorm(x,input$mu,input$sigma),type='l', col='red') 
      
    }
    
    
    # exponential
    
    if (input$cmodel == '3') {
      # exponential 
      par(mfrow=c(1,2))
      x=seq(0,input$i,0.01) 
      plot(x,dexp(x,input$lam),type='l',col='green')
      
      
    }
    
    if (input$cmodel == '1') {
      a <- input$a
      b <- input$b
      n1 <- input$s
      
      rand.unif <- runif(n1, min = a, max = b)
      
      hist(rand.unif, 
           freq = FALSE, 
           xlab = 'x',  
           ylim = c(0, 0.4),
           xlim = c(-3,3),
           density = 20,
           main = "Uniform distribution")
      
      
      curve(dunif(x, min = a, max = b), 
            from = -3, to = 3, 
            n = n1, 
            col = "darkblue", 
            lwd = 2, 
            add = TRUE, 
            yaxt = "n",
            ylab = 'probability')
      
      
    }
    
  })  
  
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  datasets <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep,header = TRUE)
    
    
  })
  
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(datasets())){return ()}
    datasets()
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table1 <- renderTable({
    if(is.null(data_column())){return ()}
    data_set()
    
  })
  
  
  
  output$selected_xdata <- ({
    renderText(input$variablexx)
  })
  
  output$selected_ydata <- ({
    renderText(input$variableyy)
  })
  
  
  output$vx1 <- renderUI({
    selectInput("variablexx" , "Select categorial Variables" ,choices =names(datasets()))
  })
  
  output$vy1 <- renderUI({
    selectInput("variableyy" , "Select Comparing Variables" ,choices =names(datasets()))
    
  })
  
  output$select_columns <- renderTable(
    {
      table(input$variablexx,input$variableyy)
    }
  )
  
  output$test_result1 <- renderPrint(
    {
      data_test = datasets()
      data_test=na.omit(data_test)
      head(data_test)
      x=data_test$min
      t=t.test(x,mu=5, alternative='less') # test of mean 
      if(t$p.value<alpha){
        decision='reject H_0'}else{
          decision='Accept H_0'
        }
      decision
      t=t.test(x,mu=5, alternative='less') # test of mean
      t
    }
  )
  
  
  output$test_result <- renderPrint(
    {
      
      
      data_test = datasets()
      
      xvalue = input$variablexx
      yvalue = input$variableyy
      
      # col1 <- paste('data_test$',xvalue,sep = "")
      #  col2 <- paste('data_test$',yvalue,sep = "")
      
      # d = table(datasets$variablexx,datasets$variablexx)
      #  d =   table(xvalue,yvalue)
      #chisq.test(d)
      
      
    }
  )
  
  #####Coding for Chi-square ################################33
  
  output$ttest <- renderPrint(
    {
      df = rnorm(input$s,input$mua,input$sd)
      t.test(df,alternative = input$value,mu =input$mu)
      
      
    }
  )
  
  
  output$plot_out <- renderPlot(
    {
      # given z values
      
      if  (input$Value == 'greater') {
        zvalue = qnorm(input$Alpha,lower.tail = FALSE)
        zvalue = as.numeric(zvalue)
        d =  pnormGC(bound=zvalue,region="above",  mean=0,sd=1,graph=TRUE)
        d
      } else if (input$Value == 'lesser')
      {
        zvalue = qnorm(input$Alpha,lower.tail = TRUE)
        zvalue = as.numeric(zvalue)
        d =  pnormGC(bound=zvalue,region="below",  mean=0,sd=1,graph=TRUE)
        d
      } else
      {
        zvalue = qnorm(input$Alpha,lower.tail = TRUE)
        zvalue = as.numeric(zvalue)
        d =  pnormGC(bound=c(-zvalue,zvalue),region="outside",  mean=0,sd=1,graph=TRUE)
        d
      }
      
      #  zvalue = qnorm(input$Alpha,lower.tail = FALSE)
      # zvalue = as.numeric(zvalue)
      # z.rg <- c(zvalue)
      #   
      #   # define cuts and canonical z-scores
      #   cuts <- sort(sort(c(z.rg, -3.5, 3.5)))  # c(-3.5, 3.5) will be the xlim of the plot
      # #  x.sq <- seq(cuts[1], cuts[4], len=200)
      #   x.sq <- seq(-10, 10, len=200)
      #   alpha <- c(.001, .01, .05)
      #   z <- c(qnorm(alpha/2), 0, abs(qnorm(alpha/2)))
      #   # plot
      #   plot(x.sq, dnorm(x.sq), type="l", xlab="z-score", ylab="density", 
      #        main="Standard Normal Distribution", xaxt="n")
      #   z <- c(qnorm(alpha/2), 0, abs(qnorm(alpha/2)))
      #   axis(1, z, round(z, 2))
      #   abline(h=0)
      #   # random middle part (optional)
      #   polygon(c(cuts[2], cuts[2], x.sq[x.sq > cuts[2] & x.sq < cuts[3]], cuts[3], cuts[3]),
      #           c(0, dnorm(cuts[2]), dnorm(x.sq[x.sq > cuts[2] & x.sq < cuts[3]]), dnorm(cuts[3]), 0),
      #           col="lightgrey", border=1)
      #   # left tail
      #   polygon(c(cuts[3], cuts[3], x.sq[x.sq > cuts[3]], cuts[4], cuts[4]),
      #           c(0, dnorm(cuts[3]), dnorm(x.sq[x.sq > cuts[3]]), dnorm(cuts[4]), 0),
      #           col="#4da6ff", border=1)
      #   # right tail
      #   polygon(c(cuts[1], cuts[1], x.sq[x.sq < cuts[2]], cuts[2], cuts[2]),
      #           c(0, dnorm(cuts[1]), dnorm(x.sq[x.sq < cuts[2]]), dnorm(cuts[2]), 0),
      #           col="#4da6ff", border=4)
      #   # labels
      #   arrows(z.rg, rep(dnorm(z.rg), length(z.rg)), z.rg, rep(dnorm(0)*.666, length(z.rg)),
      #          length=0, lty=2, col=4)
      #   sapply(z.rg, function(x) text(x, dnorm(0)*.666 + .02, bquote(italic("z=")~.(x)), col=4))
      #   
    }
  )
  
  #### code for logistic 
  dataset_logistic <- reactive({
    switch(input$data_inbuilt,
           "mtcars" = mtcars,
           "iris"  =iris,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$column_names <-renderUI({
    selectInput("col_names", label = h4("Select Dependent Variable"), 
                choices =names(dataset_logistic()))
  })
  
  output$response_variable <-renderUI({
    selectInput("response_names", label = h4("Select Dependent Variable"), 
                choices =names(dataset_logistic()),multiple = TRUE)
  })
  
  
  dataset_logcol <- reactive({
    response_names
  })
  
  
  output$response_variable_data <- renderPrint({
    
    
    library(caret)
    form <- sprintf("%s~%s",input$col_names,paste0(input$response_names,collapse="+"))
    print(form)
    
    #### split dataset for train and test data #######
    library(caTools)
    split <- sample.split(dataset_logistic(),SplitRatio =input$split)
    
    train <- subset(dataset_logistic(),split == "TRUE")
    test  <- subset(dataset_logistic(),split == "FALSE")
    
    logit_model <- glm(as.formula(form),data = train,family = "binomial")
    
    ### step 3 predict test data based on trained model - logit_model 
    fitted.result <- predict(logit_model,test ,type = "response")
    
    fitted.result  ## predicted results
    dependent_var <-  input$col_names
    test[dependent_var]   ## actual Result 
    
    ## mis classification error
    misClasserror <- mean(fitted.result !=test[dependent_var])
    #print(paste("Accuracy =",1-misClasserror))
    
    logit_model <- glm(as.formula(form),data = dataset_logistic(),family = "binomial")
    summary(logit_model)
  })
  
  output$regression_data <- renderTable({
    dataset_logistic()
  })
  
  output$test_table <- renderTable({
    
    library(caret)
    form <- sprintf("%s~%s",input$col_names,paste0(input$response_names,collapse="+"))
    print(form)
    
    #### split dataset for train and test data #######
    library(caTools)
    split <- sample.split(dataset_logistic(),SplitRatio =input$split)
    
    train <- subset(dataset_logistic(),split == "TRUE")
    
    test  <- subset(dataset_logistic(),split == "FALSE")
    
    logit_model <- glm(as.formula(form),data = train,family = "binomial")
    
    ### step 3 predict test data based on trained model - logit_model 
    fitted.result <- predict(logit_model,test ,type = "response")
    
    fitted.result  ## predicted results
    t <-   input$col_names   ## actual Result 
    test[t]
    actual_value <- test[t]
    ### change the probability to class (0 or 1 , Yes or NO )
    fitted.result <- ifelse(fitted.result > 0.5 ,1,0)
    predicted_value <- as.factor(fitted.result)
    
    
    ## mis classification error
    ##  misClasserror <- mean(fitted.result !=test[t])
    ##print(paste("Accuracy =",1-misClasserror))
    
    cc <- data.frame(predicted_value,actual_value)
    
    ## step 5 evaluate the model using confusion matrix
    
  })
  
  
  observeEvent(input$show, {
    library(caret)
    form <- sprintf("%s~%s",input$col_names,paste0(input$response_names,collapse="+"))
    print(form)
    
    #### split dataset for train and test data #######
    library(caTools)
    split <- sample.split(dataset_logistic(),SplitRatio =input$split)
    
    train <- subset(dataset_logistic(),split == "TRUE")
    
    test  <- subset(dataset_logistic(),split == "FALSE")
    
    logit_model <- glm(as.formula(form),data = train,family = "binomial")
    
    ### step 3 predict test data based on trained model - logit_model 
    fitted.result <- predict(logit_model,test ,type = "response")
    
    fitted.result  ## predicted results
    t <-   input$col_names   ## actual Result 
    test[t]
    actual_value <- test[t]
    ### change the probability to class (0 or 1 , Yes or NO )
    fitted.result <- ifelse(fitted.result > 0.5 ,1,0)
    predicted_value <- as.factor(fitted.result)
    
    
    ## mis classification error
    misClasserror <- mean(fitted.result !=test[t])
    print(paste("Accuracy =",1-misClasserror))
    
    showModal(modalDialog(
      title = "Accuracy",
      print(paste("Accuracy =",1-misClasserror))
    ))
  })
  
  ### logistic regression code for file upload 
  regression_data <- reactive({
    reg_file1 <- input$file_reg
    if(is.null(reg_file1)){return()} 
    read.table(file=reg_file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })  
  output$table_reg <- renderTable({
    if(is.null(regression_data())){return ()}
    regression_data()
  })
  ### values for columns in upload file 
  output$column_names_upd <-renderUI({
    selectInput("col_names", label = h4("Select Dependent Variable"), 
                choices =names(regression_data()))
  })
  
  output$response_variable_upd <-renderUI({
    selectInput("response_names", label = h4("Select Dependent Variable"), 
                choices =names(regression_data()),multiple = TRUE)
  })
  ####
  
  output$tb1 <- renderUI({
    tableOutput("table_reg")
  })
  
  output$reg_plot <- renderPlot({
    
    form <- sprintf("%s~%s",input$col_names,paste0(input$response_names,collapse="+"))
    print(form)
    
    #### split dataset for train and test data #######
    library(caTools)
    
    split <- sample.split(mtcars,SplitRatio =input$split)
    split
    
    ## Split the data of the above values as 80 and 20 in train and test 
    
    train <- subset(mtcars,split == "TRUE")
    
    test  <- subset(mtcars,split == "FALSE")
    
    
    ## Step 2 train model with logisic regression 
    
    logit_model <- glm(as.formula(form),data = train,family = "binomial")
    summary(logit_model)
    tt  <-   input$col_names
    fck <- test[tt]
    ROCRpred <- prediction(as.numeric(fitted.result), test[tt])
    ROCRPerf <- performance(ROCRpred,measure = "tpr" ,x.measure = "fpr")
    plot(ROCRPerf)
    plot(ROCRPerf,colorize = TRUE)
    plot(ROCRPerf,colorize = TRUE,print.cutoffs.at =seq(0.1,by=0.1))
    
    plot(ROCRPerf,colorize = TRUE,print.cutoffs.at =seq(0.1,by=0.1),main = "ROC Curve")
    abline(a = 0,b = 1)
    
    # ## AUC Area under the curve should me more and more that is alway better
    auc <- performance(ROCRpred,measure = "auc")
    auc <- auc@y.values[[1]]
    auc
    auc <- round(auc,4)
    legend(.6,.3,auc,title = "AUC",cex = 1)
    ## Code ended for plotting graph
  })
  
  
}

shinyApp(ui, server)## app.R ##
library(shinydashboard)
library(ggplot2)
library(MASS)  
require(tigerstats)
library(ROCR)
ui <- dashboardPage(
  dashboardHeader(title = "Statics dashboard"),
  dashboardSidebar(
    sidebarMenu(
      # adding menus 
      menuItem("Probability Models", tabName = "dashboard", icon = icon("dashboard"),startExpanded = TRUE,
               ## adding sub-menus for models
               menuSubItem("Discrete Model11",tabName = "discrete"),
               menuSubItem("Continuous Model",tabName = "continuous")),
      ## adding 2nd menu 
      menuItem("Hypothesis Testing", tabName = "widgets", icon = icon("th"),startExpanded = TRUE,
               ## adding sub-menus for models
               menuSubItem("Population Mean",tabName = "popmean"),
               menuSubItem("Population Portion",tabName = "poportion"),
               menuSubItem("Varience",tabName = "var")),
      menuItem("Regression Models", tabName = "regression", icon = icon("th"),startExpanded = TRUE,
               ## adding sub-menus for models
               menuSubItem("Linear Model",tabName = "linear"),
               menuSubItem("Logistic Regression Model",tabName = "logistic"),
               menuSubItem("K-NN Model",tabName = "knn"))
      
      
    )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems( 
      tabItem(
        tabName = "discrete",
        fluidRow(
          sidebarPanel(  
            selectInput("model", "Select Distribution  model",
                        c("poission" ="1"  ,"Binomial" = "2" , "Hypergeometric" = "3", 
                          "Geometric" = "4")),
            
            sliderInput("s", "sample size" ,min=0, max=1000, value = 10),
            conditionalPanel(
              "input.model == '3'",
              numericInput("nsd", "No of samples drawn urn" , value = 1),
              numericInput("ns", "No of samples in Container" , value = 1),
              numericInput("nsd2", "No of samples from urn 2" , value = 1),
              numericInput("ns2", "Sample in 2nd container" , value = 1)
            ),
            
            ## geometric model
            conditionalPanel(
              "input.model == '4'",
              numericInput("np", "Probability " , value = .1),
              numericInput("nt", "Fidn probability for Nth trail" , value = 1)
            ),
            
            
            conditionalPanel(
              "input.model == '1'",
              
              numericInput("lam", "parameter lambda in Poisson" , value = 1)),
            
            conditionalPanel(
              "input.model != '3'",
              numericInput("end", "ending value for x" , value = 10), 
              numericInput("j", "random value" , value = 1)),
            
            conditionalPanel(
              "input.model == '2'",
              
              numericInput("n", "n in Binomial" , value = 10),
              numericInput("p", "p in Binomial" , value = 0.1)),
            radioButtons("format","Download Format" ,choices = list("Pdf","png"))
            
          ),
          
          
          
          tabBox(  title = "Visualization for Probablility models",
                   # The id lets us use input$tabset1 on the server to find the current tab
                   id = "tabset1", height = "1150px",
                   
                   
                   tabPanel("Plot",
                            conditionalPanel(
                              "input.model == '1'",
                              plotOutput("plot_pois") ),
                            conditionalPanel(
                              "input.model == '2'",
                              plotOutput("plot") ),
                            
                            conditionalPanel(
                              "input.model == '3'",
                              plotOutput("plot_hyper") )
                            
                            
                   )
                   ,
                   tabPanel("Data", "Tab content 2")
          )
        )
        
      ),
      # menu tab 
      tabItem(
        tabName = "continuous",
        fluidRow(
          sidebarPanel(  
            selectInput("cmodel", "Select Continious  model",
                        c("Uniform" ="1"  ,"normal" = "2" , "exponential" = "3", 
                          "gamma" = "4", "Chi-squared" = "5" )),
            sliderInput("s", "sample size" ,min=0, max=1000, value = 10),
            
            ## Uniform model
            conditionalPanel(
              "input.cmodel == '1'",
              numericInput("a", "Starting  interval parameter" , value =1),
              numericInput("b", "Expected interval parameter" , value =1)
              
            ),
            ## Exponential 
            conditionalPanel(    
              condition = "input.cmodel == '3'",
              numericInput("lam", "parameter lambda in exponential" , value = 1) ,
              numericInput("j2", "j in exponential" , value = 0)
            ),
            ## Normal 
            conditionalPanel(
              condition = "input.cmodel == 'normal'",
              numericInput("mu", "parameter mu in Normal" , value = 0), 
              numericInput("sigma", "parameter sigma in Normal" , value = 1),
              numericInput("j1", "j in Normal" , value = 0)
            ),
            
            numericInput("i", "support" , value = 2),
            
            
            conditionalPanel(
              condition = "input.conmodel == 'uniform'",
              numericInput("a", "parameter a in Normal" , value = -2), 
              numericInput("b", "parameter b in Normal" , value = 0.8)
            )
          ),
          
          tabBox(title = "Visualization for Continious Models" ,id = "T2" , height = "1150px",
                 tabPanel("Plot", 
                          
                          
                          plotOutput("histogram") 
                          
                 ))
        )   ),
      
      tabItem(tabName = "var",
              fluidRow(
                navbarPage(strong("Varience") ,inverse = TRUE,
                           
                           tabPanel("Concept"
                                    
                           ),
                           tabPanel("Chi-Square",
                                    
                                    sidebarPanel(
                                      #  checkboxInput("dataset","Upload or use R dataset",value = TRUE),
                                      
                                      
                                      #  selectInput("dataset" , label = "Select dataset for Test", choices = c("mtcars", "iris")),
                                      br(),
                                      uiOutput("vx1"),
                                      uiOutput("vy1"),
                                      
                                      ## Selected data 
                                      tableOutput("selected_xdata"),
                                      tableOutput("selected_ydata"),
                                      #   table("selected_xdata","selected_ydata"),
                                      
                                      
                                      fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
                                      helpText("Default max. file size is 5MB"),
                                      br(),
                                      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';'), selected = ',')
                                    )
                                    ,
                                    tabBox( tabsetPanel(   
                                      tabPanel("Data",     
                                               tableOutput("table")   ),
                                      tabPanel("Results",
                                               verbatimTextOutput("test_result1") ),
                                      tabPanel("Graph")
                                    )
                                    
                                    
                                    )
                                    
                           ),
                           tabPanel("ANOVA")
                )
                
                
              )
              
      ),
      
      
      tabItem(tabName = "popmean",
              fluidRow(
                navbarPage(strong("popmean") ,inverse = TRUE,
                           
                           tabPanel("Z-table",
                                    
                                    sidebarPanel(
                                      tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                                                 tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
                                      ),
                                      numericInput("mu",label = HTML("$$ \\mu $$"), value = 10,min = 5,step = 1),
                                      br(),
                                      numericInput("xbar",HTML("$$ x&#772 $$"), value =12,min = 5,step = 1),
                                      numericInput("mua",label = HTML("$$ \\mu_1 $$"), value = 12,min = 5,step = 1),
                                      numericInput("sd","standard Devivation", value = 20,min = 5,step = 1),
                                      sliderInput("s", "sample size" ,min=0, max=1000, value = 10),
                                      radioButtons("Value","Tail Type",c("Left Tail" = "lesser","Right Tail"  = "greater","Two-Tailed" = "two.sided")),
                                      sliderInput("Alpha", "Significance Level" ,min=0, max=0.25, value = 0.5)
                                    )    ,
                                    tabBox(tabsetPanel(
                                      tabPanel("Result",
                                               fluidRow(
                                                 verbatimTextOutput("ttest"),
                                                 plotOutput("plot_out") 
                                                 
                                               ) 
                                      ),
                                      tabPanel("Z-table",
                                               plotOutput("plot_out1"))
                                    ))
                           )
                )
                
              )
      ),
      ## Regression Code
      tabItem(tabName = "logistic",
              sidebarPanel(
                selectInput("source","Select Your Source",choices = c("In-built" = "internal","Upload File" = "external")),
                conditionalPanel("input.source == 'internal'",
                                 selectInput("data_inbuilt" , label = "Select dataset for Test", choices = c("mtcars", "iris")),
                                 uiOutput("column_names"),
                                 uiOutput("response_variable"),
                                 sliderInput("split", "Split dataset" ,min=0.2, max=0.8, value = 0.8)
                ),
                conditionalPanel("input.source == 'external'",
                                 fileInput("file_reg","Upload your CSV",multiple = FALSE),
                                 uiOutput("column_names_upd"),
                                 uiOutput("response_variable_upd"),
                                 sliderInput("split", "Split dataset" ,min=0.2, max=0.8, value = 0.8),
                                 tags$hr(),
                                 h5(helpText("Select the read.table parameters below")),
                                 checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                 checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                 radioButtons(inputId = 'sep', label = 'Separator', 
                                              choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                )
                
                
              ),
              mainPanel(
                # textOutput("response_column")  
                tabsetPanel(   
                  tabPanel("Data",   
                           conditionalPanel("input.source == 'external'",
                                            tableOutput("tb1")
                           ),
                           conditionalPanel("input.source == 'internal'",
                                            tableOutput("regression_data") 
                           )
                  ),
                  tabPanel("Result" ,   
                           verbatimTextOutput("response_variable_data")  ) ,
                  tabPanel("test_result" ,   
                           tableOutput("test_table"
                           ) ,actionButton("show", "Check Accuracy Percentage") ) ,
                  tabPanel("Plot" ,   
                           plotOutput("reg_plot") 
                  )    
                  
                )
                
              )
      )
    )
    
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  output$plot <- renderPlot({
    
    par(mfrow=c(3,2))
    
    output$value <- renderPrint({ input$Model })
    # Binomial
    D1=rgeom(input$s, input$p)
    count=table(D1)
    barplot(count)
    x2=1:input$end
    y2=dgeom(x2,input$p)
    plot(x2,y2,type='b')
    
    #poisson
    D=rpois(input$s,input$lam)
    count=table(D)
    barplot(count, col='blue')
    x=0:input$end
    y=dpois(x,input$lam)
    plot(x,y,type='b')
    
    # Binomial
    D1=rbinom(input$s,input$n, input$p)
    count=table(D1)
    pie(count)
    x1=0:input$end
    y1=dbinom(x,input$n, input$p)
    plot(x1,y1,type='b')
    
  })
  
  output$plot_pois <- renderPlot({
    
    
    #poisson
    D=rpois(input$s,input$lam)
    count=table(D)
    barplot(count, col='blue')
    x=0:input$end
    y=dpois(x,input$lam)
    plot(x,y,type='b')
    
    
  })
  
  
  output$plot_hyper <- renderPlot({
    # Hypergeometric model
    H1=1-phyper(input$nsd,input$ns, input$nsd2, input$ns2)
    count=table(H1)
    pie(count)
    x1=0:input$end
    y1=dhyper(x,input$n, input$p)
    plot(x1,y1,type='b')
  })
  
  output$histogram <- renderPlot({
    
    # normal 
    if (input$cmodel == '2') {
      par(mfrow=c(1,2)) 
      x=seq(-input$i,input$i,0.01) 
      plot(x,dnorm(x,input$mu,input$sigma),type='l', col='red') 
      
    }
    
    
    # exponential
    
    if (input$cmodel == '3') {
      # exponential 
      par(mfrow=c(1,2))
      x=seq(0,input$i,0.01) 
      plot(x,dexp(x,input$lam),type='l',col='green')
      
      
    }
    
    if (input$cmodel == '1') {
      a <- input$a
      b <- input$b
      n1 <- input$s
      
      rand.unif <- runif(n1, min = a, max = b)
      
      hist(rand.unif, 
           freq = FALSE, 
           xlab = 'x',  
           ylim = c(0, 0.4),
           xlim = c(-3,3),
           density = 20,
           main = "Uniform distribution")
      
      
      curve(dunif(x, min = a, max = b), 
            from = -3, to = 3, 
            n = n1, 
            col = "darkblue", 
            lwd = 2, 
            add = TRUE, 
            yaxt = "n",
            ylab = 'probability')
      
      
    }
    
  })  
  
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  datasets <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep,header = TRUE)
    
    
  })
  
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(datasets())){return ()}
    datasets()
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table1 <- renderTable({
    if(is.null(data_column())){return ()}
    data_set()
    
  })
  
  
  
  output$selected_xdata <- ({
    renderText(input$variablexx)
  })
  
  output$selected_ydata <- ({
    renderText(input$variableyy)
  })
  
  
  output$vx1 <- renderUI({
    selectInput("variablexx" , "Select categorial Variables" ,choices =names(datasets()))
  })
  
  output$vy1 <- renderUI({
    selectInput("variableyy" , "Select Comparing Variables" ,choices =names(datasets()))
    
  })
  
  output$select_columns <- renderTable(
    {
      table(input$variablexx,input$variableyy)
    }
  )
  
  output$test_result1 <- renderPrint(
    {
      data_test = datasets()
      data_test=na.omit(data_test)
      head(data_test)
      x=data_test$min
      t=t.test(x,mu=5, alternative='less') # test of mean 
      if(t$p.value<alpha){
        decision='reject H_0'}else{
          decision='Accept H_0'
        }
      decision
      t=t.test(x,mu=5, alternative='less') # test of mean
      t
    }
  )
  
  
  output$test_result <- renderPrint(
    {
      
      
      data_test = datasets()
      
      xvalue = input$variablexx
      yvalue = input$variableyy
      
      # col1 <- paste('data_test$',xvalue,sep = "")
      #  col2 <- paste('data_test$',yvalue,sep = "")
      
      # d = table(datasets$variablexx,datasets$variablexx)
      #  d =   table(xvalue,yvalue)
      #chisq.test(d)
      
      
    }
  )
  
  #####Coding for Chi-square ################################33
  
  output$ttest <- renderPrint(
    {
      df = rnorm(input$s,input$mua,input$sd)
      t.test(df,alternative = input$value,mu =input$mu)
      
      
    }
  )
  
  
  output$plot_out <- renderPlot(
    {
      # given z values
      
      if  (input$Value == 'greater') {
        zvalue = qnorm(input$Alpha,lower.tail = FALSE)
        zvalue = as.numeric(zvalue)
        d =  pnormGC(bound=zvalue,region="above",  mean=0,sd=1,graph=TRUE)
        d
      } else if (input$Value == 'lesser')
      {
        zvalue = qnorm(input$Alpha,lower.tail = TRUE)
        zvalue = as.numeric(zvalue)
        d =  pnormGC(bound=zvalue,region="below",  mean=0,sd=1,graph=TRUE)
        d
      } else
      {
        zvalue = qnorm(input$Alpha,lower.tail = TRUE)
        zvalue = as.numeric(zvalue)
        d =  pnormGC(bound=c(-zvalue,zvalue),region="outside",  mean=0,sd=1,graph=TRUE)
        d
      }
      
      #  zvalue = qnorm(input$Alpha,lower.tail = FALSE)
      # zvalue = as.numeric(zvalue)
      # z.rg <- c(zvalue)
      #   
      #   # define cuts and canonical z-scores
      #   cuts <- sort(sort(c(z.rg, -3.5, 3.5)))  # c(-3.5, 3.5) will be the xlim of the plot
      # #  x.sq <- seq(cuts[1], cuts[4], len=200)
      #   x.sq <- seq(-10, 10, len=200)
      #   alpha <- c(.001, .01, .05)
      #   z <- c(qnorm(alpha/2), 0, abs(qnorm(alpha/2)))
      #   # plot
      #   plot(x.sq, dnorm(x.sq), type="l", xlab="z-score", ylab="density", 
      #        main="Standard Normal Distribution", xaxt="n")
      #   z <- c(qnorm(alpha/2), 0, abs(qnorm(alpha/2)))
      #   axis(1, z, round(z, 2))
      #   abline(h=0)
      #   # random middle part (optional)
      #   polygon(c(cuts[2], cuts[2], x.sq[x.sq > cuts[2] & x.sq < cuts[3]], cuts[3], cuts[3]),
      #           c(0, dnorm(cuts[2]), dnorm(x.sq[x.sq > cuts[2] & x.sq < cuts[3]]), dnorm(cuts[3]), 0),
      #           col="lightgrey", border=1)
      #   # left tail
      #   polygon(c(cuts[3], cuts[3], x.sq[x.sq > cuts[3]], cuts[4], cuts[4]),
      #           c(0, dnorm(cuts[3]), dnorm(x.sq[x.sq > cuts[3]]), dnorm(cuts[4]), 0),
      #           col="#4da6ff", border=1)
      #   # right tail
      #   polygon(c(cuts[1], cuts[1], x.sq[x.sq < cuts[2]], cuts[2], cuts[2]),
      #           c(0, dnorm(cuts[1]), dnorm(x.sq[x.sq < cuts[2]]), dnorm(cuts[2]), 0),
      #           col="#4da6ff", border=4)
      #   # labels
      #   arrows(z.rg, rep(dnorm(z.rg), length(z.rg)), z.rg, rep(dnorm(0)*.666, length(z.rg)),
      #          length=0, lty=2, col=4)
      #   sapply(z.rg, function(x) text(x, dnorm(0)*.666 + .02, bquote(italic("z=")~.(x)), col=4))
      #   
    }
  )
  
  #### code for logistic 
  dataset_logistic <- reactive({
    switch(input$data_inbuilt,
           "mtcars" = mtcars,
           "iris"  =iris,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$column_names <-renderUI({
    selectInput("col_names", label = h4("Select Dependent Variable"), 
                choices =names(dataset_logistic()))
  })
  
  output$response_variable <-renderUI({
    selectInput("response_names", label = h4("Select Dependent Variable"), 
                choices =names(dataset_logistic()),multiple = TRUE)
  })
  
  
  dataset_logcol <- reactive({
    response_names
  })
  
  
  output$response_variable_data <- renderPrint({
    
    
    library(caret)
    form <- sprintf("%s~%s",input$col_names,paste0(input$response_names,collapse="+"))
    print(form)
    
    #### split dataset for train and test data #######
    library(caTools)
    split <- sample.split(dataset_logistic(),SplitRatio =input$split)
    
    train <- subset(dataset_logistic(),split == "TRUE")
    test  <- subset(dataset_logistic(),split == "FALSE")
    
    logit_model <- glm(as.formula(form),data = train,family = "binomial")
    
    ### step 3 predict test data based on trained model - logit_model 
    fitted.result <- predict(logit_model,test ,type = "response")
    
    fitted.result  ## predicted results
    dependent_var <-  input$col_names
    test[dependent_var]   ## actual Result 
    
    ## mis classification error
    misClasserror <- mean(fitted.result !=test[dependent_var])
    #print(paste("Accuracy =",1-misClasserror))
    
    logit_model <- glm(as.formula(form),data = dataset_logistic(),family = "binomial")
    summary(logit_model)
  })
  
  output$regression_data <- renderTable({
    dataset_logistic()
  })
  
  output$test_table <- renderTable({
    
    library(caret)
    form <- sprintf("%s~%s",input$col_names,paste0(input$response_names,collapse="+"))
    print(form)
    
    #### split dataset for train and test data #######
    library(caTools)
    split <- sample.split(dataset_logistic(),SplitRatio =input$split)
    
    train <- subset(dataset_logistic(),split == "TRUE")
    
    test  <- subset(dataset_logistic(),split == "FALSE")
    
    logit_model <- glm(as.formula(form),data = train,family = "binomial")
    
    ### step 3 predict test data based on trained model - logit_model 
    fitted.result <- predict(logit_model,test ,type = "response")
    
    fitted.result  ## predicted results
    t <-   input$col_names   ## actual Result 
    test[t]
    actual_value <- test[t]
    ### change the probability to class (0 or 1 , Yes or NO )
    fitted.result <- ifelse(fitted.result > 0.5 ,1,0)
    predicted_value <- as.factor(fitted.result)
    
    
    ## mis classification error
    ##  misClasserror <- mean(fitted.result !=test[t])
    ##print(paste("Accuracy =",1-misClasserror))
    
    cc <- data.frame(predicted_value,actual_value)
    
    ## step 5 evaluate the model using confusion matrix
    
  })
  
  
  observeEvent(input$show, {
    library(caret)
    form <- sprintf("%s~%s",input$col_names,paste0(input$response_names,collapse="+"))
    print(form)
    
    #### split dataset for train and test data #######
    library(caTools)
    split <- sample.split(dataset_logistic(),SplitRatio =input$split)
    
    train <- subset(dataset_logistic(),split == "TRUE")
    
    test  <- subset(dataset_logistic(),split == "FALSE")
    
    logit_model <- glm(as.formula(form),data = train,family = "binomial")
    
    ### step 3 predict test data based on trained model - logit_model 
    fitted.result <- predict(logit_model,test ,type = "response")
    
    fitted.result  ## predicted results
    t <-   input$col_names   ## actual Result 
    test[t]
    actual_value <- test[t]
    ### change the probability to class (0 or 1 , Yes or NO )
    fitted.result <- ifelse(fitted.result > 0.5 ,1,0)
    predicted_value <- as.factor(fitted.result)
    
    
    ## mis classification error
    misClasserror <- mean(fitted.result !=test[t])
    print(paste("Accuracy =",1-misClasserror))
    
    showModal(modalDialog(
      title = "Accuracy",
      print(paste("Accuracy =",1-misClasserror))
    ))
  })
  
  ### logistic regression code for file upload 
  regression_data <- reactive({
    reg_file1 <- input$file_reg
    if(is.null(reg_file1)){return()} 
    read.table(file=reg_file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })  
  output$table_reg <- renderTable({
    if(is.null(regression_data())){return ()}
    regression_data()
  })
  ### values for columns in upload file 
  output$column_names_upd <-renderUI({
    selectInput("col_names", label = h4("Select Dependent Variable"), 
                choices =names(regression_data()))
  })
  
  output$response_variable_upd <-renderUI({
    selectInput("response_names", label = h4("Select Dependent Variable"), 
                choices =names(regression_data()),multiple = TRUE)
  })
  ####
  
  output$tb1 <- renderUI({
    tableOutput("table_reg")
  })
  
  output$reg_plot <- renderPlot({
    
    form <- sprintf("%s~%s",input$col_names,paste0(input$response_names,collapse="+"))
    print(form)
    
    #### split dataset for train and test data #######
    library(caTools)
    
    split <- sample.split(mtcars,SplitRatio =input$split)
    split
    
    ## Split the data of the above values as 80 and 20 in train and test 
    
    train <- subset(mtcars,split == "TRUE")
    
    test  <- subset(mtcars,split == "FALSE")
    
    
    ## Step 2 train model with logisic regression 
    
    logit_model <- glm(as.formula(form),data = train,family = "binomial")
    summary(logit_model)
    fitted.result <- predict(logit_model,test ,type = "response")
    tt  <-   input$col_names
    fck <- test[tt]
    ROCRpred <- prediction(as.numeric(fitted.result), test[tt])
    ROCRPerf <- performance(ROCRpred,measure = "tpr" ,x.measure = "fpr")
    plot(ROCRPerf)
    plot(ROCRPerf,colorize = TRUE)
    plot(ROCRPerf,colorize = TRUE,print.cutoffs.at =seq(0.1,by=0.1))
    
    plot(ROCRPerf,colorize = TRUE,print.cutoffs.at =seq(0.1,by=0.1),main = "ROC Curve")
    abline(a = 0,b = 1)
    
    # ## AUC Area under the curve should me more and more that is alway better
    auc <- performance(ROCRpred,measure = "auc")
    auc <- auc@y.values[[1]]
    auc
    auc <- round(auc,4)
    legend(.6,.3,auc,title = "AUC",cex = 1)
    ## Code ended for plotting graph
  })
  
  
  
  
  
  
}

shinyApp(ui, server)