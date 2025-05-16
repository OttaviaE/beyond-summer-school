server = function(input, output, session) {
  output$plot <- renderPlot({
    IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
      y <- c + (e - c) * (exp(a * (theta - b)) / (1 + exp(a * (theta - b))))
      return(y)
    }
    
    i_info <- function(b, a=1,c=0, e= 1,  theta = seq(-5,5,length.out=1000)){
      P = IRT(theta, b = b, a = a, e = e, c=c)
      Q = 1 - P 
      # Ii = (a^2)*(Q/P)*((P-c)/(e-c))^2
      # Ii = (a^2)*(Q*P/e^2)
      num = (a^2)*((P-c)^2)*((e-P)^2)
      den = ((e-c)^2)*P*Q
      Ii = num/den
      return(Ii)
    }
    
    item_info <- function(ipar, 
                          theta = seq(-5,5,length.out=1000)){
      item <- NULL
      for(i in 1:nrow(ipar)){
        item[[i]] <- i_info(b = ipar[i, "b"],a = ipar[i, "a"], c = ipar[i, "c"], e = ipar[i, "e"], theta = theta)
      }
      item = data.frame(do.call("cbind", item))
      colnames(item) = rownames(ipar)
      return(item)
    }
    theta <- seq(-7, 7, .001)
    par(mar = c(5,7,4,2) + 0.1)
    itempar = data.frame(b = c(0, 0, 0), 
                         a = c(1.2, 1.2, 1.2), 
                         c = c(0,0, 0), e = c(input$b1, input$b2, input$b3))
    
    p1 = IRT(0, b = itempar$b[1], 
             a = itempar$a[1], 
             c = itempar$c[1], 
             e = input$b1)
    p2 = IRT(0, b = itempar$b[2], 
             a = itempar$a[2], 
             c = itempar$c[2], 
             e = input$b2)
    p3 = IRT(0, b = itempar$b[3], 
             a = itempar$a[3], 
             c = itempar$c[3], 
             e = input$b3)
    plot(theta, IRT(theta, b = itempar$b[1], 
                    a = itempar$a[1], 
                    c = itempar$c[1], 
                    e = input$b1) , 
         type = "l", lwd = 2, lty = 2,
         col = "royalblue", main = "Item Characteristics Curves - ICCs", ylim = c(0,1),
         ylab = expression(paste("P(", x[p][i],  "= 1|", theta[p], ", ", b[i], ", ", a[i], ")")), 
         xlab = expression(theta), 
         cex.lab = 1.2, 
         cex.main = 1.5, 
         cex.text=1, cex.axis=1)
    
    segments(min(theta)-3, p1,
             0, p1,
             col = "royalblue", lty = 3, lwd = 1.3)
    segments(0, -p1,
             0, p1,
             col = "royalblue", lty = 3, lwd = 1.3)
    
    lines(theta, IRT(theta, 
                     b = itempar$b[2], 
                     a = itempar$a[2], 
                     c = itempar$c[2], 
                     e = input$b2),
          type = "l", lwd = 2, lty = 2,
          col = "magenta")
    lines(theta, IRT(theta, 
                     b = itempar$b[3], 
                     a = itempar$a[3], 
                     c = itempar$c[3], 
                     e = input$b3),
          type = "l", lwd = 2, lty = 2,
          col = "seagreen")
    segments(min(theta)-3, p2,
             0, p2,
             col = "magenta", lty = 3, lwd = 1.3)
    segments(0, -p2,
             0, p2,
             col = "magenta", lty = 3, lwd = 1.3)
    segments(min(theta)-3, p3,
             0, p3,
             col = "seagreen", lty = 3, lwd = 1.3)
    segments(0, -p3,
             0, p3,
             col = "seagreen", lty = 3, lwd = 1.3)
    #   
  })
  
  output$tif <- renderPlot({
    par(mar = c(5,7,4,2) + 0.1)
    itempar = data.frame(b = c(0, 0, 0), 
                         a = c(1.2, 1.2, 1.2), 
                         c = c(0,0, 0), e = c(input$b1, input$b2, input$b3))
    IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
      y <- c + (e - c) * (exp(a * (theta - b)) / (1 + exp(a * (theta - b))))
      return(y)
    }
    
    i_info <- function(b, a=1,c=0, e= 1,  theta = seq(-5,5,length.out=1000)){
      P = IRT(theta, b = b, a = a, e = e, c=c)
      Q = 1 - P 
      # Ii = (a^2)*(Q/P)*((P-c)/(e-c))^2
      # Ii = (a^2)*(Q*P/e^2)
      num = (a^2)*((P-c)^2)*((e-P)^2)
      den = ((e-c)^2)*P*Q
      Ii = num/den
      return(Ii)
    }
    
    item_info <- function(ipar, 
                          theta = seq(-5,5,length.out=1000)){
      item <- NULL
      for(i in 1:nrow(ipar)){
        item[[i]] <- i_info(b = ipar[i, "b"],a = ipar[i, "a"], c = ipar[i, "c"], e = ipar[i, "e"], theta = theta)
      }
      item = data.frame(do.call("cbind", item))
      colnames(item) = rownames(ipar)
      return(item)
    }
    theta <- seq(-7, 7, .001)
    iifs = item_info(itempar, theta = theta)
    
    plot(theta, iifs[,1], cex.lab= 2, 
         main = "Item Information Functions - IIFs",
         cex.lab = 1.2, 
         cex.main = 1.5, 
         cex.text=1, cex.axis=1,
         xlab = expression(theta), ylab = expression(paste("IIF"[i])),
         type = "l", lwd =2,
         col = "royalblue", ylim = c(0,1))
    lines(theta, iifs[,2], lwd =2,
          col = "magenta", lty = 4)
    lines(theta, iifs[,3], lwd =2,
          col = "seagreen", lty = 2)
    
    
  })
  output$tifEx = renderPlot({
    IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
      y <- c + (e - c) * (exp(a * (theta - b)) / (1 + exp(a * (theta - b))))
      return(y)
    }
    
    i_info <- function(b, a=1,c=0, e= 1,  theta = seq(-5,5,length.out=1000)){
      P = IRT(theta, b = b, a = a, e = e, c=c)
      Q = 1 - P 
      # Ii = (a^2)*(Q/P)*((P-c)/(e-c))^2
      # Ii = (a^2)*(Q*P/e^2)
      num = (a^2)*((P-c)^2)*((e-P)^2)
      den = ((e-c)^2)*P*Q
      Ii = num/den
      return(Ii)
    }
    
    item_info <- function(ipar, 
                          theta = seq(-5,5,length.out=1000)){
      item <- NULL
      for(i in 1:nrow(ipar)){
        item[[i]] <- i_info(b = ipar[i, "b"],a = ipar[i, "a"], c = ipar[i, "c"], e = ipar[i, "e"], theta = theta)
      }
      item = data.frame(do.call("cbind", item))
      colnames(item) = rownames(ipar)
      return(item)
    }
    theta <- seq(-7, 7, .001)
    par(mar = c(5,7,4,2) + 0.1)
    itempar = data.frame(b = c(0, 0, 0), 
                         a = c(1.2, 1.2, 1.2), 
                         c = c(0,0, 0), e = c(input$b1, input$b2, input$b3))
    
    new_par = itempar
    new_par$e = rep(1, nrow(new_par))
    tif = item_info(new_par, theta = theta)
    tif$tif = rowSums(tif)
    plot(theta, tif$tif, type = "l", 
         lwd = 1.3, col = "black", 
         cex.lab = 1.2, 
         cex.main = 1.5, 
         cex.text=1, cex.axis=1, xlab = expression(theta), 
         ylab = "TIF", 
         main = "Test Information Function - TIF")
    tifTired = item_info(itempar, theta = theta)
    tifTired$tired = rowSums(tifTired)
    lines(theta, tifTired$tired, type = "l", lwd = 1.3, col = "firebrick")
  })
}