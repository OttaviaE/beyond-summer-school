server = function(input, output, session) {
IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  return(y)
}
# calcola l'IIF per un item specifico
i_info <- function(b, a=1,c=0, e= 1,  theta = seq(-5,5,length.out=1000)){
  Ii = (a^2)*IRT(theta, b = b, a = a, e = e )*(1- IRT(theta, b = b, a = a, e = e ))
  return(Ii)
}
# calcola l'IIF di tutti gli item e restituisce in una lista di lunghezza ugaule a tutti 
# gli item per cui si è calcolata l'IIF
item_info <- function(ipar, theta = seq(-5,5,length.out=1000)){
  item <- NULL
  if (any(colnames(ipar) == "e")) {
    for(i in 1:nrow(ipar)){
      item[[i]] <- i_info(b = ipar[i, "b"],a = ipar[i, "a"], e = ipar[i, "e"], theta = theta)
    } 
  } else {
    for(i in 1:nrow(ipar)){
      item[[i]] <- i_info(b = ipar[i, "b"],a = ipar[i, "a"], theta = theta)
    }
  }
  item = data.frame(do.call("cbind", item))
  colnames(item) = rownames(ipar)
  return(item)
}
  output$plot <- renderPlot({
    theta <- seq(-7, 7, .001)
    par(mar = c(5,7,4,2) + 0.1)
    b = c(input$b1, input$b2, input$b3)
    a = c(input$a1, input$a2, input$a3)
    
    
    plot(theta, IRT(theta, b = input$b1, a = input$a1), 
         type = "l", lwd = 2, lty = 2,
         col = "royalblue", main = "Item Characteristics Curves - ICCs", 
         ylab = expression(paste("P(", x[p][i],  "= 1|", theta[p], ", ", b[i], ", ", a[i], ")")), 
         xlab = expression(theta), 
         cex.lab = 1.2, 
         cex.main = 1.5, 
         cex.text=1, cex.axis=1)
    
    segments(min(theta)-3, 0.5, 
             input$b1, 0.5, 
             col = "gray56", lty = 3, lwd = 1.3)
    segments(input$b1, -0.5, 
             input$b1, 0.5, 
             col = "royalblue", lty = 3, lwd = 1.3)
    
    lines(theta, IRT(theta, b=input$b2,
                     a=input$a2),
          type = "l", lwd = 2, lty = 2,
          col = "magenta")
    lines(theta, IRT(theta, b=input$b3,
                     a=input$a3),
          type = "l", lwd = 2, lty = 2,
          col = "seagreen")
    segments(min(theta)-3, 0.5, 
             input$b2, 0.5, 
             col = "gray56", lty = 3, lwd = 1.3)
    segments(input$b2, -0.5, 
             input$b2, 0.5, 
             col = "magenta", lty = 3, lwd = 1.3)
    segments(min(theta)-3, 0.5, 
             input$b3, 0.5, 
             col = "gray56", lty = 3, lwd = 1.3)
    segments(input$b3, -0.5, 
             input$b3, 0.5, 
             col = "seagreen", lty = 3, lwd = 1.3)
  })
  
  output$tif <- renderPlot({
    
    Theta <- matrix(seq(-4,4, length.out=1000))
    par(mar = c(5,7,4,2) + 0.1)
    parameters = data.frame(b = c(input$b1, input$b2, input$b3), a = c(input$a1, input$a2, input$a3))
    iifs = item_info(parameters, Theta)  
    plot(Theta, iifs[,1], cex.lab= 2, main = "Item Information Functions - IIFs",
         cex.lab = 1.2, 
         cex.main = 1.5, 
         cex.text=1, cex.axis=1, 
         xlab = expression(theta), ylab = expression(paste("IIF"[i])),
         type = "l", lwd =2,
         col = "royalblue", ylim = c(0,1))
    lines(Theta, iifs[,2], lwd =2,
          col = "magenta", lty = 4)
    lines(Theta, iifs[,3], lwd =2,
          col = "seagreen", lty = 2)
    
    if (input$showTif == TRUE) {
      plot(Theta, rowSums(iifs),
           type = "l", lwd =2,
           col = "black", ylim = c(0, 2),
           xlab = expression(theta), 
           ylab = expression(paste("I(", theta, ")")), 
           main = "IIFs and Test Information Functions",  
           cex.lab = 1.2, 
           cex.main = 1.5, 
           cex.text=1, cex.axis=1, )
      lines(Theta, iifs[,1], lwd =2,
            col = "royalblue", lty = 4)
      lines(Theta, iifs[,2], lwd =2,
            col = "magenta", lty = 4)
      lines(Theta, iifs[,3], lwd =2,
            col = "seagreen", lty = 2)
    }
  })
  
  
}