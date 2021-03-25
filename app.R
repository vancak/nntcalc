#### libraries ####
library(shiny)

library(nntcalc)
library(png)

####user interface####
ui <- fluidPage( titlePanel(title=div(img(height = "100%",
                                          width  = "100%",
                                          src    = "NNT_LOGO.png",
                                          align  = "left"))),
                 titlePanel( h1( strong("Number Needed to Treat (NNT) Calculator"), align = "center" ) ),
                  h3(strong( "An online calculator of the unadjusted, adjusted,
                     and the marginal NNT with the corresponding 95% confidence intervals"),
                     align = "center"),
                 h4( "For user guidlines and step-by-step tutorial please refer to the following paper",
                    align = "center"),
                 h4( tags$a("Guidelines to understand and compute the number needed to treat",
                             href = "http://dx.doi.org/10.1136/ebmental-2020-300232", ),
                    align = "center"),

     #            uiOutput("tab1"),
fluidRow(
  column(4,
                 uiOutput("tab2") ),
  column(2,
                 uiOutput("tab3") ),
  column(2,
                 uiOutput("tab21") ),
  column(2,
                 uiOutput("tab32") ),
  column(2, 
                 uiOutput("tab4") ) ),
                 
                 fluidRow(
                 column(6,

                 selectInput(inputId = "dataset",
                             label = "Select a sample dataset (please type a name with the filename extension when saving)",
                             choices = c("unadjusted NNT",
                                         "adjusted NNT for ANOVA model",
                                         "adjusted NNT for linear regression",
                                         "adjusted NNT for logistic regression",
                                         "adjusted NNT for Cox regression"
                             )),
         #       br(),

                 downloadButton("downloadData", "Download")),

         #        tableOutput('sample'),
                 
                 column(6,

                  fileInput("file1",
                            "Choose a csv file (please reload the page before uploading each file)",
                            multiple = FALSE,
                            accept   = c("text/csv",
                                       "text/comma-separated-values, text/plain",
                                       ".csv")),

                 checkboxInput("header", "Header", TRUE))),

                  ### UNADJUSTED NNT ###

                  titlePanel( h3( strong("UNADJASTED NNT"), align = "center" ) ),
                   h4("Calculates the unadjusted Laupacis type NNT (NNT L) or Kraemer & Kupfer's type NNT (KK-NNT)
                  with the corresponding 95% confidence intervals.
                  Please choose the NNT type, the subsequent required fields, and then press Run.",
                                 align = "center"),

                  div(style="display:inline-block",
                      selectInput(inputId = "nnt_type",
                                  "Select the NNT type",
                                   choices = c("",
                                              "Unadjusted Laupacis NNT",
                                              "Kraemer & Kupfer KK-NNT") ) ),

                  div(style="display:inline-block",
                      selectInput('treat',
                      'Select the treatment arm',
                      "") ),

                  div(style="display:inline-block",
                      selectInput('control',
                      'Select the control arm',
                     "") ),

                  div(style="display:inline-block",
                      selectInput(inputId = "nnt_est",
                                    label = "Select the estimator",
                                  choices = c("",
                                              "Nonparametric MLE",
                                              "Parametric MLE",
                                              "Furukawa & Leucht")) ) ,


                  selectInput(inputId = "dist",
                              label = "Select the distribution",
                              choices = c("",
                                          "Normal",
                                          "Exponential",
                                          "Unknown")),

                  div(style="display:inline-block",
                      numericInput(inputId = "mcid1",
                                   label   = "Insert the MCID threshold",
                                   value   = NA)),

                  withTags(div(class='row-fluid',
                               div(style="display:inline-block",
                                checkboxInput( inputId = "eq_var",
                                     label   = "Equal variances",
                                     TRUE)),

                  div(style="display:inline-block",
                      checkboxInput( inputId = "decrease1",
                                     label   = "Success - decrease",
                                     TRUE )) )),

                  titlePanel( ("") ),

                  ###### ADJUSTED NNT ######

                  titlePanel( h3( strong("ADJUSTED NNT"), align = "center") ),
                   h4("Calculates the adjusted and the marginal Laupacis type NNT
                   with the corresponding 95% confidence intervals.
                  Please choose the regression model,
                  the subsequent required fields,
                  and then press Run.",
                                 align = "center"),

                  selectInput(inputId = "reg_mod",
                              label = "Select the model",
                              choices = c("",
                                          "one-way ANOVA",
                                          "linear regression",
                                          "logistic regression",
                                          "Cox regression")),

                  div(style="display:inline-block",
                      selectInput('dep_var',
                                  'Select the dependent variable',
                                  "") ),

                  div(style="display:inline-block",
                      selectInput('adj_var',
                                  "Select the independent variable for adjustment",
                             "") ),

                div(style="display:inline-block",
                    selectInput('group_id',
                                'Select the group ID variable',
                                "") ),

                div(style="display:inline-block",
                    selectInput('status',
                                "Select the status variable (Survival analysis)",
                                "") ),

                 numericInput(inputId = "adj_value",
                              label   = "Insert a specific value for adjustment",
                              value   = NA),

                div(style="display:inline-block",
                    numericInput(inputId = "mcid2",
                                 label   = "Insert the MCID threshold",
                                 value   = NA)),

                div(style="display:inline-block",
                       textInput(inputId = "base_gr",
                                 label   = "Insert the reference group (ANOVA)",
                                 value   = NA)),

                div(style="display:inline-block",
                    numericInput(inputId = "time_point",
                              label   = "Insert the time point (Survival analysis)",
                              value   = NA)),

                    checkboxInput( inputId = "decrease2",
                                   label   = "Success - decrease",
                                   TRUE ),

                actionButton("run_button","Run"),

                downloadButton("download", "Download results in csv format"),

                tableOutput('contents'),
                
                h2(""),

                # submitButton("Run"),

                h5("Designed and developed by Valentin Vancak", align = "center"),
                h5(tags$a(href = "https://github.com/vancak/NNTcalculator",
                          "This application is based on the nntcalc R package"),
                   align = "center"),


                )

##### SERVER #####

server <- function( input, output, session ) {
  #Reactive to store loaded data
  reactives <- reactiveValues(
    
    dat = NULL
    
  )
  # 
  #   ext <- tools::file_ext(input$file$name)
  # if(ext!="csv"){
  #   showNotification("wassup")
  # }
  
   observeEvent(input$file1,{
    
     reactives$dat =     read.csv(file    = input$file1$datapath)
     
    

         updateSelectInput(session,
                           inputId  = 'treat',
                           label    = 'Select the treatment arm',
                           choices  = c( "", names(reactives$dat)))

         updateSelectInput(session,
                           inputId  = 'control',
                           label    = "Select the control arm",
                           choices  = c( "", names(reactives$dat)))

         updateSelectInput(session,
                           inputId  = 'dep_var',
                           label    = "Select the dependent variable",
                           choices  = c( "", names(reactives$dat)))

         updateSelectInput(session,
                           inputId  = 'adj_var',
                           label    = "Select the independent variable for adjustment",
                           choices  = c( "", names(reactives$dat)))

         updateSelectInput(session,
                           inputId  = 'group_id',
                           label    = "Select the group ID variable",
                           choices  = c("", names(reactives$dat)))

         updateSelectInput(session,
                           inputId  = 'status',
                           label    = "Select the status variable (Survival analysis)",
                           choices  = c("", names(reactives$dat)))


       })


  out_data <- reactive( { withProgress(message = 'Calculating the required NNT... Please wait',
                                                 value = 0.8,
                                                 {
                                                   
req(input$run_button)
    if( input$nnt_type == "Unadjusted Laupacis NNT" &
        input$nnt_est  == "Nonparametric MLE" )
    {
      return( as.data.frame( nnt_l( type      = "laupacis",
                     treat     = reactives$dat[,input$treat],
                     control   = reactives$dat[,input$control],
                     cutoff    = input$mcid1,
                     decrease  = input$decrease1,
                     dist      = "none"), row.names = "" ) )

      }


    if( input$nnt_type == "Unadjusted Laupacis NNT" &
        input$nnt_est  == "Parametric MLE" )
    {
      return( as.data.frame( nnt_l( type      = "mle",
                     treat     = reactives$dat[,input$treat],
                     control   = reactives$dat[,input$control],
                     cutoff    = input$mcid1,
                     decrease  = input$decrease1,
                     dist      = ifelse(input$dist == "Normal", "normal",
                                 ifelse(input$dist == "Exponential", "expon", "") ),
                     equal.var = input$eq_var ), row.names = "" ) )
          }

     if( input$nnt_type == "Unadjusted Laupacis NNT" &
         input$nnt_est  == "Furukawa & Leucht" )
  {
    return( as.data.frame( nnt_l( type      = "fl",
                   treat     = reactives$dat[,input$treat],
                   control   = reactives$dat[,input$control],
                   cutoff    = input$mcid1,
                   decrease  = input$decrease1,
                   dist      = 'normal' ), row.names = "" ) )
     }

    if( input$nnt_type == "Kraemer & Kupfer KK-NNT" &
        input$nnt_est  == "Nonparametric MLE" )
    {
      return( as.data.frame( nnt_kk( type      = "non-param",
                      treat     = reactives$dat[,input$treat],
                      control   = reactives$dat[,input$control],
                      decrease  = input$decrease1,
                      dist      = 'none' ), row.names = "" ) )
    }

    if( input$nnt_type == "Kraemer & Kupfer KK-NNT" &
        input$nnt_est  == "Parametric MLE")
    {
      return( as.data.frame( nnt_kk( type      = "mle",
                      treat     = reactives$dat[,input$treat],
                      control   = reactives$dat[,input$control],
                      decrease  = input$decrease1,
                      dist      = ifelse(input$dist == "Normal", "normal",
                                         ifelse(input$dist == "Exponential", "expon", "") ),
                      equal.var = input$eq_var), row.names = "" ) )
    }

######## ADJUSTED NNT ########
    if( input$reg_mod == "one-way ANOVA" )
    {
      return(  nnt_x(  model     = "anova",
                       response  = reactives$dat[,input$dep_var],
                       x         = reactives$dat[,input$adj_var],
                       cutoff    = input$mcid2,
                       base      = input$base_gr,
                       decrease  = input$decrease2,
                       data      = reactives$dat) )
    }

    if( input$reg_mod == "linear regression" )
    {
      return(  nnt_x(  model     = "linreg",
                       response  = reactives$dat[,input$dep_var],
                       x         = reactives$dat[,input$adj_var],
                       cutoff    = input$mcid2,
                       group     = reactives$dat[,input$group_id],
                       decrease  = input$decrease2,
                       adj       = ifelse( !is.na(input$adj_value),
                                           input$adj_value,
                                           round(mean(reactives$dat[,input$adj_var], na.rm = T), 2) ),
                       data      = reactives$dat) )
    }

    if( input$reg_mod == "logistic regression" )
    {
      return(  nnt_x(  model     = "logreg",
                       response  = reactives$dat[,input$dep_var],
                       x         = reactives$dat[,input$adj_var],
                       group     = reactives$dat[,input$group_id],
                       adj       = ifelse( !is.na(input$adj_value),
                                           input$adj_value,
                                           round(mean(reactives$dat[,input$adj_var], na.rm = T), 2) ),
                       data      = reactives$dat) )
    }

    if( input$reg_mod == "Cox regression" )
    {
      return(  nnt_survreg(  response   = reactives$dat[,input$dep_var],
                             status     = reactives$dat[,input$status],
                             x          = reactives$dat[,input$adj_var],
                             group      = reactives$dat[,input$group_id],
                             adj        = ifelse( !is.na(input$adj_value),
                                                  input$adj_value,
                                                  round(mean(reactives$dat[,input$adj_var], na.rm = T), 2) ),
                             time.point = input$time_point,
                             data       = reactives$dat) )
    }

    else

      return( data.frame(Note = "Please make sure all fields are filled in correctly",
                         row.names = ""  ) )

     } )} )

 # , include.rownames = TRUE
  output$contents <- renderTable({
    validate({
      req(input$file1)
      if(tools::file_ext(input$file1$name)=="csv")
        NULL
      else("Please upload a csv file")}
    )
    
    
   
      out_data()}
    , rownames = TRUE)

  ### DOWNLOAD DATA ###
  output$download <-
    downloadHandler(
      filename = function () {
        paste("NNT_results.csv", sep = "")
      },
      content = function(file) {
        write.csv(out_data(), file)
      }
    )



  ### DOWNLOAD SAMPLE DATA ###
  sampleData <- reactive({ 
    # if(input$dataset == "unadjusted NNT") {
    #     return( read.csv(".//data//panss_unadjusted.csv") ) }
    switch(input$dataset,
           "unadjusted NNT"                       = read.csv(".//data//panss_unadjusted.csv"),
           "adjusted NNT for ANOVA model"         = read.csv(".//data//anova_data.csv"),
           "adjusted NNT for linear regression"   = read.csv(".//data//panss_regression.csv"),
           "adjusted NNT for logistic regression" = read.csv(".//data//panss_logistic.csv"),
           "adjusted NNT for Cox regression"      = read.csv(".//data//panss_survival.csv")
    )
    #     else
    #         if(input$dataset == "adjusted NNT for ANOVA model") {
    #             return( read.csv(".//data//anova_data.csv") ) }
    # 
  })
  
  output$downloadData <- downloadHandler( 
    filename = function(){
      paste("sample_dataset.csv", sep = "")
    },
    
    content = function(file) {
      write.csv(sampleData(), file, row.names = FALSE)
    })
  
  output$download <- downloadHandler( 
    filename = function(){
      paste("results.csv", sep = "")
    },
    
    content = function(file) {
      write.csv(out_data(), file, row.names = FALSE)
    })


}

# Run the application
shinyApp(ui = ui, server = server)

