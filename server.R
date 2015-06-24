# server.R

source("helpers.R")

shinyServer(
  function(input, output) {
    
    output$poweroutput <- renderPrint({
      
      LRPowerCorr(sampsize = input$n, nsims = input$nsims, p = input$rho, alpha = input$alpha,
                   pcx1 = input$pcx1, pcx2 = input$pcx2,
                   org1 = input$org1, org2 = input$org2,
                   k1 = input$k1, k2 = input$k2
                   )
      
    })
    
  }
)
