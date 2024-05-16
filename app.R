library(shiny)
library(tidyverse)
library(lme4) 
library(shinycssloaders)
library(progress)
library(bslib)
library(openxlsx) # export excel


source("rutgers.optimal.cat.blocks.random.ver.1.R")
source("orthonormal.contrasts.R")
source("generate_factor_input_panel.R")


# Define UI for D-optimal Design app ----
ui <- navbarPage( 
  
  id = "inTabset",
  
  theme = bs_theme(version = version_default(), bootswatch = "minty"),
  
  title = strong("DOE"),
  
  # add text on the right top of navbar, this will cause a Warning:
  # "Navigation containers expect a collection of `bslib::nav_panel()......"
  # but never mind.
  tags$script(HTML("var header = $('.navbar> .container-fluid');
                       header.append('<div style=\"float:right\">Designed by Yaxuan He, Yicong Li</div>');
                       console.log(header)")),


  # App title ----
  tabPanel("D-optimal Design", 
       # Sidebar layout with input and output definitions ----
       sidebarLayout(
  
           # Sidebar panel for inputs ----
           sidebarPanel(
             width = 5,
             
         
             # Add button ----
             actionButton("design", "Make Design", class = "btn-dark btn-lg", width = "100%",
                          icon = icon("play",lib = "glyphicon"),),
             
             br(),
             br(),
             
             # Add Download Button
             downloadButton("saveRDS","Save Input as RDS", class = "btn-primary", style = "width:50%;"),
             
             downloadButton("downloadData", "Download Excel", class = "btn-primary", style = "width:49%;"),
             
             br(),
             br(),
             
             fileInput("upload", "Choose RDS File to Upload Previous Input:", accept = ".rds"),
             tags$script(HTML("
                $(document).ready(function(){
                $('#upload').parent().removeClass('btn btn-default btn-file').addClass('btn btn-primary btn-file');
                 });
             ")),
             br(),
             # actionButton("remove_file", "Remove File"),
             
             # Add tab for the input, this will cause a lot of Warning but never mind.
             tabsetPanel(
               tabPanel(
                 "Basic Settings",
                 
                 br(),
                 
                 # Input: number of factors
                 numericInput(
                   inputId = "numberfactors",
                   min = 1,
                   max = NA,
                   2,
                   label = "Number of Factors:"
                 ),
                 
                 br(),
                 
                 # Input: Formulations ----
                 textInput("in.frml", "Input Formulation:", value = ""),
                 helpText("The default setting will be Full Factorial Design."),
                 actionButton("helpButton2", "", icon = icon("question-sign", lib = "glyphicon"),
                              style="color: #5d5b5b; background-color: #f7f7f7; border-color: #f7f7f7;
                           height: calc(0%); width:calc(7%); border-radius: 0px; border-width: 0px"),
                 br(),
                 br(),
                 
                 # Input: Specify the number of runs ----
                 numericInput("n.runs", "Numbers of Runs:", 12, min = 1),
                 helpText("The default value represents the minimum number of runs. You may increase the number as per your need."),
                 actionButton("helpButton1", "", icon = icon("question-sign", lib = "glyphicon"),
                              style="color: #5d5b5b; background-color: #f7f7f7; border-color: #f7f7f7;
                         height: calc(0%); width:calc(7%); border-radius: 0px; border-width: 0px"),
                 br(),
                 br(),
                 
                 generate_factor_input_panel(1),
                 uiOutput("additional_factors"),
                 
                 br(),
                 
               ),
               
               
               tabPanel(
                 "Advanced Arguments",
                 br(),
                 
                 # Input: Specify the number of blocks ----
                 numericInput("blocks", "Numbers of Blocks:", 0, min = 0),
                 checkboxInput("random", label = "Random Blocks", value = FALSE),
                 helpText("You can leave this input blank if you don't need to specify the blocks."),
                 br(),
                 br(),
                 
                 
                 # Input: Specify the number of runs per blocks ----
                 numericInput("size", "How many Runs per Blocks:", 0, min = 0),
                 helpText("You can leave this input blank if you didn't specify the previous input \"Numbers of Blocks\"."),
                 br(),
                 br(),
                 
                 h6("Number of Runs:"),
                 textOutput("out.runs"),
                 br(),
                 
                 # number of random starts for the optimization algorithm
                 numericInput("r.starts", "Numbers of Random Starts for the optimization algorithm:", 10, min = 1),
                 br(),
                 
                 # number of iterations per random start
                 numericInput("iter", "Numbers of Iterations per random start:", 1000, min = 1),
                 br(),
               )
             ),
          ),
           
           
      # Main panel for displaying outputs ----
      mainPanel(
        width = 7,
        
        h3("Design Evaluation"),
        textOutput("time"),
        br(),
        
        tabsetPanel(
          
          # Output: Header + table of Design Matrix ----
          tabPanel("X Matrix", br(),tableOutput("matrix")),
          
          # Output: Header + table of the subset of experimental runs that maximize the D-optimal criterion ----
          tabPanel("Run List", br(),tableOutput("finalRuns")),
          
          # Output: Header + result of Design Diagnostics ----
          tabPanel("Design Diagnostics",br(), tableOutput("eff")),
          
          # Output: Header + result of Power Analysis ---- 
          tabPanel("Power Analysis",br(), tableOutput("result")),
          
          # Output: Header + result of FDS plot ---- 
          tabPanel("FDS Plot", br(), plotOutput("fds", width = "80%")),
          
        ),
      )
    ),
    

    tags$head(
      tags$style(
        HTML(".shiny-notification {
                height: calc(10%);
                width: calc(35%);
                position:fixed;
                top: calc(40%);;
                left: calc(48%);;
                font-size: 18px;;
              }
             "
        )
     )
    )
  
   ),
  
  # Help document
  tabPanel("Help", icon = icon("question-sign", class = NULL, lib = "glyphicon"),
    
     value = "helpPanel",
           
     fluidRow(
    
     column(3),
    
      column(6,
       
       h2("Help Document"),
       br(),
       strong("Welcome to the help section for the D-optimal Design app!"),
       br(),
       br(),
       
      p("Design of Experiment (DOE) is a vital methodology in the development of new drugs and vaccines, particularly in the establishment of biological assays. 
      These assays play a crucial role in assessing the efficacy and safety of pharmaceutical compounds. 
      DOE techniques offer an efficient approach to identify and optimize critical factors that contribute to the performance and robustness of biological assays."),
   
       
       p("Moreover, DOE finds wide application in various domains including engineering, manufacturing, healthcare, agriculture, and social sciences. It serves several key purposes:"),
     tags$ul(
        tags$li("Identifying Important Factors: DOE helps identify the key factors that affect the outcome of interest and determine which factors have the most significant impact."),
        tags$li("Optimizing Processes: By systematically varying factors and observing their effects on the response, DOE enables the optimization of processes to achieve desired outcomes efficiently."),
        tags$li("Reducing Variability: DOE helps understand the sources of variability in a process or system and develop strategies to reduce or control it."),
        tags$li("Improving Quality: By optimizing processes and reducing variability, DOE can lead to improved product quality, increased productivity, and cost savings.")
      ),
     
       # Adding linking local file
       p("For more detailed insights into the theoretical foundations of DOE, you may refer to the document ", 
         a(href='Note_DOE.pdf', target='blank', 'Design of Exeriment'), "."),
  
       
       br(),
       strong("And this app helps you create D-optimal designs for your experiments. Follow these steps to use the app:"),
       br(),
     
       br(),
       h4("Step 1: Specify the number of numeric factors"),
       p("First starting with the", strong("Basic Settings"), ". The default number of numeric factors is", code("2"), "but you can enter any number you want."),
       
       br(),
       h4("Step 2: Set the specific factors"),
       p("After selecting the number of factors, corresponding panels for each factor will appear. 
         You can customize your own numeric factors by naming them as you wish, and setting the low and high values. "),
       p("Here, you can choose only ONE factor as categorical factor with multiple levels. The default level is", code("0"), 
         "which means you do not add any categorical factor. If you want to add a categorical factor, 
         you need to choose at least 2 or more levels to enable contrast."),
       
       br(),
       h4("Step 3: Specify the number of runs"),
       p("This input control the number of runs to generate."),
       p("The number of runs will be calculated automatically, to ensure that the total number of runs is larger than", 
         code("(2^(number of numeric factors)) * level of categorical factor"), 
         "This default number meets the minimum requirement to prevent singular system errors, 
         but it does NOT ensure it won't lead to singular system error.
         Thus it's recommended to adjust it based on your specific needs, potentially increasing it for better results."),
       
    
     

       br(),
       h4("Step 4: Input your formulation (optional)"), 
       p("This is an optional input. The default setting will be a full model, 
       including the factors and their interaction terms plus the optional blocks, 
       i.e.,", code("~a^2 + b^2 + a:b"), "or", code("~(a + b)^2"), "for only 2 numeric factors' design setting. 
       If you want to define your own formulation, you may enter, for example,", code("~ a + b + c + a*(b+c)"), 
         "for 3 factors' design setting."),
       p("An example of a custom formulation is shown in the following picture:"),
       HTML('<center><img src="f_eg.png" style="width: calc(60%);"></center>'),

       br(),
     
       br(),
       h4("Step 5: Customize Advanced Arguments Setting (optional)"),
       p("The following setting is under the tab panel", strong("Advanced Arguments"),"."), 
       p("If you want to add blocks to your experiment, specify the number of 
         blocks and the number of runs per block. Also you can click the",code("Random Blocks"),"checkbox to use 
          use random effects when modeling the blocks, instead of the default Fixed Blocks."),
       p("If you're not using blocks, you can leave the input", code("Number of Blocks"), 
       "and", code("Runs per Block"), "blank, and the total number of runs will automatically output as the text under them."),
       p("Also, you may enter your own settings in the input section", code("Number of Random Starts for the optimization algorithm"), 
         "and", code("Number of Iterations per random start"), "or leave it with the default value."),
       
       
       br(),
       h4("Step 6: Click 'Make Design' to generate the design"),
       p("Voila! Now, wait a few seconds for the design result. The progress bar will show you the process."),
       p("Then, view the output: Design Matrix, Run List, Design Diagnostics (including D efficiency, A efficiency, G efficiency, 
         and Average Variance of Prediction), Power Analysis, and FDS Polt in the main panel. And the Design Creation Time
         will be shown under the title of Design Evaluation."),
       
       br(),
       h4("Step 7: Save your work & Upload later"),
       p("You have two convenient options for saving your work:"),
       p(strong("Save Input as RDS"),": Use the 'Save Input as RDS' button to save your input settings as an RDS file. 
         Later, you can upload this file by clicking 'Browse...' under the save button."),
       p(strong("Download Excel"),": Utilize the 'Download Excel' 
         button to obtain an Excel file containing all your input settings and 
         output results, including the comprehensive FDS plot."),
       br(),
       br(),
        ),
     
     
     column(3),
   )
  )
)




# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  
  # Click the Help icon then jump to the second Tab
  observeEvent(input$helpButton1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "helpPanel")
  })
  
  # Click the Help icon then jump to the second Tab
  observeEvent(input$helpButton2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "helpPanel")
  })
  
  
  
  # Generate the factor_input_panel
  factor_input_cache = reactiveValues()
  
  ui_elements = reactive({
    req(factor_input_cache, cancelOutput = TRUE)
    ui_elements_list = list()
    if(input$numberfactors > 1) {
      for(i in seq_len(input$numberfactors)[-1]) {
        ui_elements_list[[i-1]] = generate_factor_input_panel(i, reactiveValuesToList(factor_input_cache))
      }
    }
    do.call(tagList, ui_elements_list)
  }
  ) |>
    bindEvent(updated_ui_defaults())
  

  
  updated_ui_defaults = reactive({
    
    for(i in seq_len(input$numberfactors)) {
      factorname_n = sprintf("factorname%i",i)
      factortype_n = sprintf("factortype%i",i)
      numericlow_n = sprintf("numericlow%i",i)
      numerichigh_n = sprintf("numerichigh%i",i)
      
      if(is.null(factor_input_cache[[factorname_n]])) {
        factor_input_cache[[factorname_n]]  = sprintf("X%i",i)
        factor_input_cache[[factortype_n]]  = "Numeric"
        factor_input_cache[[numericlow_n]]  = -1
        factor_input_cache[[numerichigh_n]] = 1
        factor_input_cache$levels           = 2
      } else {
        factor_input_cache[[factorname_n]]  = input[[factorname_n]]
        factor_input_cache[[factortype_n]]  = input[[factortype_n]]
        factor_input_cache[[numericlow_n]]  = input[[numericlow_n]]
        factor_input_cache[[numerichigh_n]] = input[[numerichigh_n]]
        factor_input_cache$levels           = input$levels
      }
      
      
    }
    
    input$numberfactors
  }) |>
    bindEvent(input$numberfactors) 
    #debounce(100)
  
  
  output$additional_factors = renderUI({
    ui_elements()
  })
  
  
  # reload the rds file
  observe({
    req(input$upload)
    ext <- tools::file_ext(input$upload$datapath)
    validate(need(ext == "rds", "Please upload an RDS file"))
    
    # read RDS file
    loaded_data <- reactiveFileReader(1000, session, filePath = input$upload$datapath, readFunc = readRDS)
    
    if(loaded_data()$n.cat == 0){
      updateNumericInput(session, "numberfactors", value = loaded_data()$n.fact)
    } else {
      updateNumericInput(session, "numberfactors", value = loaded_data()$n.fact + 1)
    }
    
    # update
    updateTextInput(session, "in.frml", value = loaded_data()$in.frml)
    updateNumericInput(session, "size", value = loaded_data()$size)
    updateNumericInput(session, "blocks", value = loaded_data()$blocks)
    updateNumericInput(session, "r.starts", value = loaded_data()$r.starts)
    updateNumericInput(session, "iter", value = loaded_data()$iter)
    updateNumericInput(session, "n.runs", value = loaded_data()$n.runs)
    updateCheckboxInput(session, "random", value = loaded_data()$random.blocks)
    
    
    for(i in seq_len(loaded_data()$n.fact)) {
      factorname_n  = sprintf("factorname%i",i)
      factortype_n  = sprintf("factortype%i",i)
      numericlow_n  = sprintf("numericlow%i",i)
      numerichigh_n = sprintf("numerichigh%i",i)
      
      updateTextInput(session, factorname_n, value = loaded_data()$num_namelist[i])
      updateSelectInput(session, factortype_n, selected = "numeric")
      updateNumericInput(session, numericlow_n, value = loaded_data()$num_values[[i]][1])
      updateNumericInput(session, numerichigh_n, value = loaded_data()$num_values[[i]][2])
    }  
    
    if(loaded_data()$n.cat > 0){
      factortype_n  = sprintf("factortype%i", loaded_data()$n.fact + 1)
      factorname_n  = sprintf("factorname%i", loaded_data()$n.fact + 1)
      # debug: when the type of the last factor doesn't change, this may because 
      # the app need sometime to react, when the number of factors increase
      # one solution is to reduce the debounce number, or print something as below
      cat("cat_type",factortype_n,input[[factortype_n]],"\n")
      cat("cat_name",factorname_n,input[[factorname_n]],"\n")
      
      updateSelectInput(session, factortype_n, selected = "cat")
      updateTextInput(session, factorname_n, value = loaded_data()$cat_name)
      updateNumericInput(session, "levels", value = loaded_data()$n.cat)
    }
    
    # unlink(input$upload$datapath)
    # remove uploaded file 
    # file.remove(input$upload$datapath)
  })
  
  # # Observe the Remove File button clicks 
  # observeEvent(input$remove_file, { 
  #   # Check if a file has been uploaded 
  #   if (!is.null(input$upload)) { 
  #     # Attempt to remove the file, and check if removal was successful 
  #     if (file.exists(input$upload$datapath)) { 
  #       file_remove_success <- file.remove(input$upload$datapath) 
  #       if (file_remove_success) { 
  #         showNotification("File removed successfully!", type = "message")
  #       } else { showNotification("Failed to remove file.", type = "error") } 
  #     } else { showNotification("No file to remove.", type = "error") } 
  #   } 
  # }) 
  
  
  
  ## Set the warning of the categorical factor setting more than 1
  observe({
    catCount <- 0
    for (i in seq_len(input$numberfactors)) {
      factortype_n <- sprintf("factortype%i", i)
      # Check if the input is not empty and is equal to "cat"
      if (!is.null(input[[factortype_n]]) && input[[factortype_n]] == "cat") {
        catCount <- catCount+1
        # debug:
        # print(paste0("number of cat factors", catCount))
      }
      if (catCount > 1) {
        showNotification("Please select no more than ONE categorical factor.", type = "error", duration = 3)
      }
    }
  })
  

  
  # update the default number of runs after input the number of factors
  observe({
    if(input$blocks > 0){
      number.runs <- input$size * input$blocks
    } else{
      if(length(input$levels) != 0){
        number.runs <- 2^(input$numberfactors-1) * input$levels + 1
      }else{
        number.runs <- 2^input$numberfactors + 1
      }
    }
    
    updateNumericInput(session, "n.runs", value = number.runs)
    
  })
  
  # update the default formulation after input the number of factors
  observe({
    namelist <- c()
    for(i in seq_len(input$numberfactors)) {
      factorname_n = sprintf("factorname%i",i)
      namelist <- c(namelist, input[[factorname_n]])
    }
    default.frml <- sprintf("~(%s)^%s", paste(namelist[1:input$numberfactors], collapse = "+"), input$numberfactors)
    if (input$blocks > 1 & !input$random){
      default.frml <- sprintf("%s+Blocks", default.frml)
    }
    
    updateTextInput(session, "in.frml", value = default.frml)
  })
  
  output$out.runs <- renderText({input$n.runs})
  
  opt.result <- reactiveVal(list(matrix = "Waiting...", eff = "Waiting...", finalRuns = "Waiting...", 
                                 power = "Waiting...", fds = "Waiting", time = "will be shown later..."))
  
  output$matrix <- renderTable(
    opt.result()$matrix
  )
  
  output$finalRuns <- renderTable(
    opt.result()$finalRuns
  )
  
  output$eff <- renderTable(
    opt.result()$eff,
    colnames = FALSE
  )
  
  output$result <- renderTable(
    opt.result()$power,
    digits = 3
  )
  
  output$fds <- renderPlot(
    opt.result()$fds
  )
  
  output$time <- renderText(
    paste("Design Creation Time (seconds):", opt.result()$time)
  )
  


  
  observeEvent(input$design, {
    
    isolate({
    
    n.fact       <- 0
    num_namelist <- c()
    num_values   <- list()
    n.cat        <- 0
    cat_name     <- NULL
    
    for(i in seq_len(input$numberfactors)) {
      factorname_n  = sprintf("factorname%i",i)
      factortype_n  = sprintf("factortype%i",i)
      numericlow_n  = sprintf("numericlow%i",i)
      numerichigh_n = sprintf("numerichigh%i",i)
      
      
      if(input[[factortype_n]] == "numeric"){
        n.fact       <- n.fact + 1
        num_namelist <- c(num_namelist, input[[factorname_n]])
        num_values   <- append(num_values,
                               list(c(input[[numericlow_n]], input[[numerichigh_n]])))
      } else{
        n.cat    <- input$levels
        cat_name <- input[[factorname_n]]
      }
    }
    
    in.frml       <- input$in.frml
    size          <- input$size
    blocks        <- input$blocks          
    r.starts      <- input$r.starts  
    iter          <- input$iter  
    n.runs        <- input$n.runs 
    random.blocks <- input$random
    
    })
    
    #Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Making design", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance : value <- value + (progress$getMax() - value) / 5
    # - If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + progress$getMax()/ (0.9*r.starts)
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute the new data, and pass in the updateProgress function so
    # that it can update the progress indicator.
    opt.result(rutgers.optimal.cat.blocks.random(n.fact=n.fact, num_namelist=num_namelist, num_values=num_values,
                                                 n.cat=n.cat, cat_name=cat_name, in.frml=in.frml, 
                                                 size=size, blocks = blocks, r.starts = r.starts,
                                                 iter = iter, n.runs=n.runs, random.blocks=random.blocks,
                                                 updateProgress=updateProgress))
  })
  
  
  # Save RDS file
  output$saveRDS <- downloadHandler(
    filename = function() {
      paste("D_Optimal_Design_", format(Sys.time(), "%Y%m%d%H%M%S"), ".rds", sep = "")
    },
    
    content = function(file) {
      isolate({
        
        n.fact       <- 0
        num_namelist <- c()
        num_values   <- list()
        n.cat        <- 0
        cat_name     <- NULL
        
        for(i in seq_len(input$numberfactors)) {
          factorname_n  = sprintf("factorname%i",i)
          factortype_n  = sprintf("factortype%i",i)
          numericlow_n  = sprintf("numericlow%i",i)
          numerichigh_n = sprintf("numerichigh%i",i)
          
          
          if(input[[factortype_n]] == "numeric"){
            n.fact       <- n.fact + 1
            num_namelist <- c(num_namelist, input[[factorname_n]])
            num_values   <- append(num_values,
                                   list(c(input[[numericlow_n]], input[[numerichigh_n]])))
          } else{
            n.cat    <- input$levels
            cat_name <- input[[factorname_n]]
          }
        }
        
        in.frml       <- input$in.frml
        size          <- input$size
        blocks        <- input$blocks          
        r.starts      <- input$r.starts  
        iter          <- input$iter  
        n.runs        <- input$n.runs 
        random.blocks <- input$random
        
      })
      
      save_rds <-   list(n.fact        = n.fact, 
                         num_namelist  = num_namelist, 
                         num_values    = num_values,
                         n.cat         = n.cat, 
                         cat_name      = cat_name, 
                         in.frml       = in.frml, 
                         size          = size, 
                         blocks        = blocks, 
                         r.starts      = r.starts,
                         iter          = iter, 
                         n.runs        = n.runs, 
                         random.blocks = random.blocks
                         )
      
      print(save_rds)
      saveRDS(save_rds, file)
    }
  )
  
  
  # Downloadable Excel file with multiple sheets
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("D_Optimal_Design_", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      # use lapply to extend all factors into same length
      max_length <- max(lengths(opt.result()$inputFct))
      inputFct <- lapply(opt.result()$inputFct, function(x) {
        if(length(x) < max_length) {
          c(x, rep(NA, max_length - length(x)))
        } else {x}
      })
      input_factors_df <- data.frame(inputFct, stringsAsFactors = FALSE)
      
      # Extract Input setting
      input_details_df <- data.frame(input = c("Numbers of Blocks:", "How many Runs per Blocks:",
                                       "Numbers of Runs:", "Input Formulation:",
                                       "Numbers of Random Starts:", "Numbers of Iterations per random start:"),
                             value = c(input$size, input$blocks,
                                       input$n.runs, input$in.frml,
                                       input$r.starts, input$iter))
      
      # Extract relevant data frames from opt.result()
      matrix_df <- as.data.frame(opt.result()$matrix)
      finalRuns_df <- as.data.frame(opt.result()$finalRuns)
      power_df <- as.data.frame(opt.result()$power)
      eff_df <- as.data.frame(rbind(opt.result()$eff, 
                                    c("Design Creation Time (seconds)", opt.result()$time)))
      
      # Create Excel workbook
      wb <- createWorkbook()
      
      # Add sheets to the workbook
      addWorksheet(wb, "InputFactors")
      addWorksheet(wb, "InputDetails")
      addWorksheet(wb, "DesignMatrix")
      addWorksheet(wb, "FinalRuns")
      addWorksheet(wb, "PowerAnalysis")
      addWorksheet(wb, "DesignDiagnostics")
      
      # Write data frames to each sheet
      writeData(wb, sheet = "InputFactors", x = input_factors_df)
      writeData(wb, sheet = "InputDetails", x = input_details_df)
      writeData(wb, sheet = "DesignMatrix", x = matrix_df)
      writeData(wb, sheet = "FinalRuns", x = finalRuns_df)
      writeData(wb, sheet = "PowerAnalysis", x = power_df)
      writeData(wb, sheet = "DesignDiagnostics", x = eff_df)
      
      # Insert plot as background in a new sheet
      addWorksheet(wb, "FDS_Plot")
      print(opt.result()$fds)
      insertPlot(wb, sheet = "FDS_Plot")
      
      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  

}

# Create Shiny app ----
shinyApp(ui=ui, server=server)
