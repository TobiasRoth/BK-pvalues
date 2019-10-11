rm(list=ls())
setwd("~/Desktop")


#-------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#

### CI ###

daten_CI <- data.frame()

for(i in 1:1000) {
  ok <- TRUE
  while(ok) {
    n <- 100
    a <- rbinom(1, 1, 0.5)
    m1 <- runif(1, -1, 1.25)
    m2 <- m1 * (a * runif(1, 1.5, 2)  + (abs(a - 1)) * runif(1, 0.2, 0.5))
    sd1 <- runif(1, 2, 3.5)
    sd2 <- sd1 * (a * runif(1, 1.5, 2)  + (abs(a - 1)) * runif(1, 0.2, 0.5))
    sample1 <- rnorm(n, m1, sd1)
    sample2 <- rnorm(n, m2, sd2)
    t1 <- t.test(sample1)
    t2 <- t.test(sample2)
    ok <- t1$p.value < 0.001 & t2$p.value < 0.001
  }
  
  pdf(paste0("BK_Umfrage_CI/BK_Umfrage_CI_", i, ".pdf"), width = 5, height = 3)
  par(mar = c(0, 2, 2, 0))
  
      plot(1:2, c(mean(sample1), mean(sample2)), ylim = c(-4, 4), axes = FALSE, 
           xlab = "", ylab = "", xlim = c(0.5, 2.5), pch = 16)
      axis(2, las = 1, at = -4:4, labels = rep("", 9))
      lines(c(1, 1), t1$conf.int)
      lines(c(2, 2), t2$conf.int)
      mtext(paste("p", ifelse(t1$p.value < 0.001, "< 0.001", paste("=", signif(t1$p.value, digits = 2)))), side = 3, at = 1)
      mtext(paste("p", ifelse(t2$p.value < 0.001, "< 0.001", paste("=", signif(t2$p.value, digits = 2)))), side = 3, at = 2)
      
      plot(1:2, c(mean(sample1), mean(sample2)), ylim = c(-4, 4), axes = FALSE, 
           xlab = "", ylab = "", xlim = c(0.5, 2.5), pch = 16)
      axis(2, las = 1, at = -4:4, labels = rep("", 9))
      lines(c(1, 1), t1$conf.int)
      lines(c(2, 2), t2$conf.int)
      
      daten_CI[i, "m1"] <- m1
      daten_CI[i, "m2"] <- m2
      daten_CI[i, "sd1"] <- sd1
      daten_CI[i, "sd2"] <- sd2
      daten_CI[i, "mean1"] <- mean(sample1)
      daten_CI[i, "mean2"] <- mean(sample2)
      daten_CI[i, "md1"] <- median(sample1)
      daten_CI[i, "md2"] <- median(sample2)
      daten_CI[i, "length_CI1"] <- t1$conf.int[2] - t1$conf.int[1]
      daten_CI[i, "length_CI2"] <- t2$conf.int[2] - t2$conf.int[1]
      daten_CI[i, "p_value1"] <- t1$p.value
      daten_CI[i, "p_value2"] <- t2$p.value
  
  dev.off()
}

write.csv(daten_CI, file = "daten_CI.csv")

#-------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#

### Boxplot ###

daten_BP <- data.frame()

for(i in 1:1000) {
  ok <- TRUE
  while(ok) {
    n <- 100
    a <- rbinom(1, 1, 0.5)
    m1 <- runif(1, -1, 1.25)
    m2 <- m1 * (a * runif(1, 1.5, 2)  + (abs(a - 1)) * runif(1, 0.2, 0.5))
    sd1 <- runif(1, 2, 3.5)
    sd2 <- sd1 * (a * runif(1, 1.5, 2)  + (abs(a - 1)) * runif(1, 0.2, 0.5))
    sample1 <- rnorm(n, m1, sd1)
    sample2 <- rnorm(n, m2, sd2)
    t1 <- t.test(sample1)
    t2 <- t.test(sample2)
    ok <- t1$p.value < 0.001 & t2$p.value < 0.001
  }
  
    pdf(paste0("BK_Umfrage_BP/BK_Umfrage_BP_", i, ".pdf"), width = 5, height = 3)
    par(mar = c(0, 2, 2, 0))
    
    boxplot(sample1, sample2, axes = F, ylim = c(-16, 19), xlim = c(0,4), at = c(1,3))
    axis(2, las = 1, at = seq(-16, 19, 4), labels = rep("", 9))
    mtext(paste("p", ifelse(t1$p.value < 0.001, "< 0.001", paste("=", signif(t1$p.value, 2)))), side = 3, at = 1)
    mtext(paste("p", ifelse(t2$p.value < 0.001, "< 0.001", paste("=", signif(t2$p.value, 2)))), side = 3, at = 3)
    
    boxplot(sample1, sample2, axes = F, ylim = c(-16, 19), xlim = c(0,4), at = c(1,3))
    axis(2, las = 1, at = seq(-16, 19, 4), labels = rep("", 9))
    
    daten_BP[i, "m1"] <- m1
    daten_BP[i, "m2"] <- m2
    daten_BP[i, "sd1"] <- sd1
    daten_BP[i, "sd2"] <- sd2
    daten_BP[i, "mean1"] <- mean(sample1)
    daten_BP[i, "mean2"] <- mean(sample2)
    daten_BP[i, "md1"] <- median(sample1)
    daten_BP[i, "md2"] <- median(sample2)
    daten_BP[i, "length_CI1"] <- t1$conf.int[2] - t1$conf.int[1]
    daten_BP[i, "length_CI2"] <- t2$conf.int[2] - t2$conf.int[1]
    daten_BP[i, "p_value1"] <- t1$p.value
    daten_BP[i, "p_value2"] <- t2$p.value
 
  dev.off()
}

write.csv(daten_BP, file = "daten_BP.csv")

#-------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#



# ?remove
# ?round
# ?ifelse
# ?signif
# ? return