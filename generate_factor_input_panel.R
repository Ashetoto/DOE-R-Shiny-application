generate_factor_input_panel = function(factor_n = 2, factor_input_cache = NULL) {

  factorname_n = sprintf("factorname%i",factor_n)
  factortype_n = sprintf("factortype%i",factor_n)
  numericlow_n = sprintf("numericlow%i",factor_n)
  numerichigh_n = sprintf("numerichigh%i",factor_n)

  default_val = function(input, val) ifelse(length(input) == 0, val, input)
  
  single_panel = shiny::wellPanel(#style = panelstyle,
                                  shiny::h5(sprintf("Factor %i", factor_n)),
                                  shiny::fluidRow(
                                    shiny::column(width = 6,
                                                  shiny::selectInput(inputId = sprintf("factortype%i",factor_n),
                                                                     choices = list("Numeric" = "numeric", "Categorical" = "cat"),
                                                                     selected = default_val(factor_input_cache[[factortype_n]], "Numeric"),
                                                                     label = "Type")
                                                
                                    ),
                                    shiny::column(width = 6,
                                                  shiny::textInput(inputId = sprintf("factorname%i",factor_n),
                                                                   value = default_val(factor_input_cache[[factorname_n]], sprintf("X%i", factor_n)),
                                                                   label = "Name")
                                                
                                                  
                                    )
                                  ),
                                  
                                
                                  shiny::conditionalPanel(
                                    condition = sprintf("input.factortype%i == \'numeric\'",factor_n),
                                    shiny::fluidRow(
                                      shiny::column(width = 6,
                                                    shiny::numericInput(inputId = sprintf("numericlow%i",factor_n),
                                                                        value =  default_val(factor_input_cache[[numericlow_n]], -1),
                                                                        label = "Low")
                                      ),
                                      shiny::column(width = 6,
                                                    shiny::numericInput(inputId = sprintf("numerichigh%i",factor_n),
                                                                        value = default_val(factor_input_cache[[numerichigh_n]], 1),
                                                                        label = "High")
                                      ),
                    
                                    )
                                  ),
     
                                  
                                  shiny::conditionalPanel(
                                    condition = sprintf("input.factortype%i == \'cat\'",factor_n),
                                    shiny::fluidRow(
                                      shiny::column(width = 12,
                                                    shiny::numericInput(inputId = "levels",
                                                                     value = default_val(factor_input_cache$levels, 2),
                                                                     label = "Levels")
                                      )
                                    )
                                  )
  )
 
  return(single_panel)
}