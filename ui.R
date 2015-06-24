# ui.R

shinyUI(fluidPage(
  titlePanel("Power Estimation Associated with a Logistic Regression Interaction Term in the 2 x 2 x 2 Case"),
  
#   helpText("Power estimation associated with a logistic regression interaction term in the 2 x 2 x 2 case:"),

   hr(),

   helpText(
     h4(
       div("This is an R Shiny web application that estimates power associated with a logistic regression interaction term
        in the 2 x 2 x 2 case.  This means that the response variable (Y) has only two possible outcomes and that there are
        two predictor variables (X1 and X2) such that each one only has two levels.  X1 is the main predictor of interest,
        while X2 may be an effect modifier.  For this web application, it is important to use the following layout for
        the two 2 x 2 tables.   Let Y=1 be the outcome of interest for the response variable; let X1=1 be the characteristic of
        interest for X1; and let X2=1 be the characteristic of interest for X2.", style = "color:black")
     )
     ),

   fluidRow(
     column(2,
            tags$table(border="1", align="center",
              tags$caption(style = "text-align:center", strong(
                                  div("X2=0", style = "color:black")
                                  )
                           ),
              tags$tr(
                tags$th(colspan="2", rowspan="2", " "),
                tags$th(style = "text-align:center", colspan="2", "X1"),
                tags$th(" ")
              ),
              tags$tr(
                tags$td(align="center", "0"),
                tags$td(align="center", "1"),
                tags$td(align="center", "Total")
              ),
              tags$tr(
                tags$th(align="center", rowspan="2", "Y"),
                tags$td(align="center", "0"),
                tags$td(align="center", "a"),
                tags$td(align="center", "b"),
                tags$td(align="center", "a+b")
              ),
              tags$tr(
                tags$td(align="center", "1"),
                tags$td(align="center", "c"),
                tags$td(align="center", "d"),
                tags$td(align="center", "c+d")
              ),
              tags$tr(
                tags$td(" "),
                tags$td(align="center", "Total"),
                tags$td(align="center", "a+c"),
                tags$td(align="center", "b+d"),
                tags$td(align="center", "n")
              )
            )
     ),
     
     column(2,
            tags$table(border="1", align="center",
                       tags$caption(style = "text-align:center", strong(
                                           div("X2=1", style = "color:black")
                                           )
                                    ),
                       tags$tr(
                         tags$th(colspan="2", rowspan="2", " "),
                         tags$th(style = "text-align:center", colspan="2", "X1"),
                         tags$th(" ")
                       ),
                       tags$tr(
                         tags$td(align="center", "0"),
                         tags$td(align="center", "1"),
                         tags$td(align="center", "Total")
                       ),
                       tags$tr(
                         tags$th(align="center", rowspan="2", "Y"),
                         tags$td(align="center", "0"),
                         tags$td(align="center", "A"),
                         tags$td(align="center", "B"),
                         tags$td(align="center", "A+B")
                       ),
                       tags$tr(
                         tags$td(align="center", "1"),
                         tags$td(align="center", "C"),
                         tags$td(align="center", "D"),
                         tags$td(align="center", "C+D")
                       ),
                       tags$tr(
                         tags$td(" "),
                         tags$td(align="center", "Total"),
                         tags$td(align="center", "A+C"),
                         tags$td(align="center", "B+D"),
                         tags$td(align="center", "N")
                       )
            )
     ),
     
     column(2,
            helpText(" ")
     ),
     
     column(2,
            helpText(" ")
     ),
     
     column(2,
            helpText(" ")
     ),
     
     column(2,
            helpText(" ")
     )
     
   ),

  hr(),

  helpText(
    h3(
      div("Example:", style = "color:black")
    )
  ),

  helpText(
    h4(
      div("Suppose there is some binary quality of care outcome (Y) of interest in long-term care facilities.   The main 
        predictor of interest (X1) is whether or not a person is obese, and a possible effect modifier of interest is a binary
        race variable (X2) where X2=1 corresponds to Race 1 and X2=0 corresponds to Race 0.  Suppose that
        it is known that 60% of the population of interest is obese (regardless of race) and that 15% of the population
        of interest is Race 1.  Further, one conjectures that the odds ratio associated with the binary
        outcome of interest with respect to the obesity predictor for Race 0 is 2 and that the corresponding odds ratio of
        interest for Race 1 is 4.  One would like to estimate the power to detect this difference in the two odds ratios based
        on a likelihood ratio test approach, such as logistic regression.  To use this app, one needs to conjecture a
        proportionality constant, k1, relating cell counts b and d above (the cell counts for group '1' for X1 when
        X2=0) where d=k1*b and also a proportionality constant, k2, relating cell counts B and D (the cell counts for group '1'
        for X1 when X2=1) where D=k2*B.  In this example, k1=2 and k2=4 will be specified.  Finally, let's specify a 
        total sample size of n + N = 2600, 1000 simulations, a correlation of 0 between X1 and X2, and the size of the test as alpha=0.05.  The
        results appear at the bottom of the page.", style = "color:black")
    )
  ),  

  fluidRow(
    column(2,
         tags$table(border="1", align="center",
                    tags$caption(style = "text-align:center", strong(
                                        div("X2=0 (Race 0)", style = "color:black")
                                        )
                                 ),
                    tags$tr(
                      tags$th(colspan="2", rowspan="2", " "),
                      tags$th(style = "text-align:center", colspan="2", "X1 (Obese)"),
                      tags$th(" ")
                    ),
                    tags$tr(
                      tags$td(align="center", "0 (No)"),
                      tags$td(align="center", "1 (Yes)"),
                      tags$td(align="center", "Total")
                    ),
                    tags$tr(
                      tags$th(align="center", rowspan="2", "Y"),
                      tags$td(align="center", "0 (No)"),
                      tags$td(align="center", "a"),
                      tags$td(align="center", "b"),
                      tags$td(align="center", "a+b")
                    ),
                    tags$tr(
                      tags$td(align="center", "1 (Yes)"),
                      tags$td(align="center", "c"),
                      tags$td(align="center", "d"),
                      tags$td(align="center", "c+d")
                    ),
                    tags$tr(
                      tags$td(" "),
                      tags$td(align="center", "Total"),
                      tags$td(align="center", "a+c"),
                      tags$td(align="center", "b+d"),
                      tags$td(align="center", "n")
                    )
         )
    ),
  
    column(2,
         tags$table(border="1", align="center",
                    tags$caption(style = "text-align:center", strong(
                                        div("X2=1 (Race 1)", style = "color:black")
                                        )
                                 ),
                    tags$tr(
                      tags$th(colspan="2", rowspan="2", " "),
                      tags$th(style = "text-align:center", colspan="2", "X1 (Obese)"),
                      tags$th(" ")
                    ),
                    tags$tr(
                      tags$td(align="center", "0 (No)"),
                      tags$td(align="center", "1 (Yes)"),
                      tags$td(align="center", "Total")
                    ),
                    tags$tr(
                      tags$th(align="center", rowspan="2", "Y"),
                      tags$td(align="center", "0 (No)"),
                      tags$td(align="center", "A"),
                      tags$td(align="center", "B"),
                      tags$td(align="center", "A+B")
                    ),
                    tags$tr(
                      tags$td(align="center", "1 (Yes)"),
                      tags$td(align="center", "C"),
                      tags$td(align="center", "D"),
                      tags$td(align="center", "C+D")
                    ),
                    tags$tr(
                      tags$td(" "),
                      tags$td(align="center", "Total"),
                      tags$td(align="center", "A+C"),
                      tags$td(align="center", "B+D"),
                      tags$td(align="center", "N")
                    )
         )
    ),
    
    column(2,
           helpText(" ")
    ),
    
    column(2,
           helpText(" ")
    ),
    
    column(2,
           helpText(" ")
    ),
    
    column(2,
           helpText(" ")
    )
    
  ),

  hr(),
  
  fluidRow(
    column(3,
      numericInput("n", 
                  label = "Choose a total sample size (n + N)",
                  value = 2600,
                  min = 1),
      
      numericInput("nsims", 
                   label = "Choose the number of simulations",
                   value = 100,
                   min = 1),
      
      numericInput("rho", 
                   label = "Specify the correlation between X1 and X2",
                   value = 0,
                   min = -1,
                   max = 1),
      
      numericInput("alpha", 
                   label = "Specify alpha",
                   value = 0.05,
                   min = 0,
                   max = 1),
      
      br(),
      br(),
      br(),
      
      submitButton(text = "Submit")
      
      ),
    column(3,
      numericInput("pcx1", 
                        label = "Specify the proportion for X1=1, regardless of level of X2",
                        value = 0.6,
                        min = 0,
                        max = 1),
           
      numericInput("pcx2", 
                        label = "Specify the proportion for X2=1",
                        value = 0.15,
                        min = 0,
                        max = 1)
      ),
    column(3,  
      numericInput("org1", 
                   label = "Specify the odds ratio for the X2=0 group",
                   value = 2,
                   min = 0),
      
      numericInput("k1", 
                   label = "Specify the k1 constant",
                   value = 2,
                   min = 0.00001)
      ),
    column(3,
      numericInput("org2", 
                   label = "Specify the odds ratio for the X2=1 group",
                   value = 4,
                   min = 0),
      
      numericInput("k2", 
                   label = "Specify the k2 constant",
                   value = 4,
                   min = 0.00001)
      )
  ),
    
  hr(),
  
  #helpText("Estimated power"),
    
  verbatimTextOutput("poweroutput")

))
