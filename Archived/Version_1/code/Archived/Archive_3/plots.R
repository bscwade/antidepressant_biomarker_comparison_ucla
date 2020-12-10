## PvA plots ##
idx <- 2
plot(c(unlist(sapply(results[[idx]], function(x) x$predicted))), 
     c(unlist(sapply(results[[idx]], function(x) x$actual))),
     pch=19,
     cex=.75,
     ylab='Actual RRS-Reflection Change',
     xlab='Predicted RRS-Reflection Change')

abline(lm(c(unlist(sapply(results[[idx]], function(x) x$actual))) ~ 
            c(unlist(sapply(results[[idx]], function(x) x$predicted)))), 
       col='red', 
       lwd=2)



















