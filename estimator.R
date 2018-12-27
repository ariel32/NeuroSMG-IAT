remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

library(tidyverse)

# filename = "result.csv"

do.estimation <- function(filename) {
  d.src = read.csv(filename, sep = ";", encoding = "UTF-8", header = F)
  names(d.src) <- c("session", "name","time","block","n","duration", "side","i","mark","error","term")
  
  d.src$time <- as.numeric(d.src$time)
  d.src$session <- as.character(d.src$session)
  
  for(x in unique(d.src$session)) {
    # x = unique(d.src$session)[1]
    print(x)
    
    d <- d.src[d.src$session==x,]
    result <- data.frame(session = d$session[1], name = d$name[1])
    
    # data quality - errors
    result$errors.all         <- length(d$error[d$error == "true"])/length(d$error[d$error == "false"])
    result$errors.b1.all      <- length(d$error[d$block == 1 & d$error == "true"])/length(d$error[d$block == 1 & d$error == "false"])
    result$errors.b1.soft     <- length(d$error[d$block == 1 & d$error == "true" & d$mark == "soft"])/length(d$error[d$block == 1 & d$error == "false" & d$mark == "soft"])
    result$errors.b1.alco     <- length(d$error[d$block == 1 & d$error == "true" & d$mark == "alco"])/length(d$error[d$block == 1 & d$error == "false" & d$mark == "alco"])
    
    result$errors.b2.all      <- length(d$error[d$block == 2 & d$error == "true"])/length(d$error[d$block == 2 & d$error == "false"])
    result$errors.b2.good     <- length(d$error[d$block == 2 & d$error == "true" & d$mark == "good"])/length(d$error[d$block == 2 & d$error == "false" & d$mark == "good"])
    result$errors.b2.bad      <- length(d$error[d$block == 2 & d$error == "true" & d$mark == "bad"])/length(d$error[d$block == 2 & d$error == "false" & d$mark == "bad"])
    
    result$errors.b3.all      <- length(d$error[d$block == 3 & d$error == "true"])/length(d$error[d$block == 3 & d$error == "false"])
    result$errors.b3.goodsoft <- length(d$error[d$block == 3 & d$error == "true" & d$mark == "goodsoft"])/length(d$error[d$block == 3 & d$error == "false" & d$mark == "goodsoft"])
    result$errors.b3.badalco  <- length(d$error[d$block == 3 & d$error == "true" & d$mark == "badalco"])/length(d$error[d$block == 3 & d$error == "false" & d$mark == "badalco"])
    
    result$errors.b4.all      <- length(d$error[d$block == 4 & d$error == "true"])/length(d$error[d$block == 4 & d$error == "false"])
    result$errors.b4.goodsoft <- length(d$error[d$block == 4 & d$error == "true" & d$mark == "goodsoft"])/length(d$error[d$block == 4 & d$error == "false" & d$mark == "goodsoft"])
    result$errors.b4.badalco  <- length(d$error[d$block == 4 & d$error == "true" & d$mark == "badalco"])/length(d$error[d$block == 4 & d$error == "false" & d$mark == "badalco"])
    
    result$errors.b5.all      <- length(d$error[d$block == 5 & d$error == "true"])/length(d$error[d$block == 5 & d$error == "false"])
    result$errors.b5.bad      <- length(d$error[d$block == 5 & d$error == "true" & d$mark == "bad"])/length(d$error[d$block == 5 & d$error == "false" & d$mark == "bad"])
    result$errors.b5.good     <- length(d$error[d$block == 5 & d$error == "true" & d$mark == "good"])/length(d$error[d$block == 5 & d$error == "false" & d$mark == "good"])
    
    result$errors.b6.all      <- length(d$error[d$block == 6 & d$error == "true"])/length(d$error[d$block == 6 & d$error == "false"])
    result$errors.b6.goodsoft <- length(d$error[d$block == 6 & d$error == "true" & d$mark == "badsoft"])/length(d$error[d$block == 6 & d$error == "false" & d$mark == "badsoft"])
    result$errors.b6.badalco  <- length(d$error[d$block == 6 & d$error == "true" & d$mark == "goodalco"])/length(d$error[d$block == 6 & d$error == "false" & d$mark == "goodalco"])
    
    result$errors.b7.all      <- length(d$error[d$block == 7 & d$error == "true"])/length(d$error[d$block == 7 & d$error == "false"])
    result$errors.b7.goodsoft <- length(d$error[d$block == 7 & d$error == "true" & d$mark == "badsoft"])/length(d$error[d$block == 7 & d$error == "false" & d$mark == "badsoft"])
    result$errors.b7.badalco  <- length(d$error[d$block == 7 & d$error == "true" & d$mark == "goodalco"])/length(d$error[d$block == 7 & d$error == "false" & d$mark == "goodalco"])
    
    ################################################################################
    # data quality for ALL results - fast and slow answers
    d$tick <- c(NA,diff(d$time))
    d.all <- d[d$error == "false",]
    result$fast.ans    <- sum(d.all$tick < 300, na.rm = T)
    result$slow.ans    <- sum(d.all$tick > 10000, na.rm = T)
    
    # time intervals
    result$time.b1.soft     <- median(d.all$tick[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)
    result$time.b1.alco     <- median(d.all$tick[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    
    result$time.b2.good     <- median(d.all$tick[d.all$block== 2 & d.all$mark == "good"], na.rm = T)
    result$time.b2.bad      <- median(d.all$tick[d.all$block== 2 & d.all$mark == "bad"], na.rm = T)
    
    result$time.b3.goodsoft <- median(d.all$tick[d.all$block== 3 & d.all$mark == "goodsoft"], na.rm = T)
    result$time.b3.badalco  <- median(d.all$tick[d.all$block== 3 & d.all$mark == "badalco"], na.rm = T)
    
    result$time.b4.goodsoft <- median(d.all$tick[d.all$block== 4 & d.all$mark == "goodsoft"], na.rm = T)
    result$time.b4.badalco  <- median(d.all$tick[d.all$block== 4 & d.all$mark == "badalco"], na.rm = T)
    
    result$time.b5.bad      <- median(d.all$tick[d.all$block== 5 & d.all$mark == "bad"], na.rm = T)
    result$time.b5.good     <- median(d.all$tick[d.all$block== 5 & d.all$mark == "good"], na.rm = T)
    
    result$time.b6.badsoft  <- median(d.all$tick[d.all$block== 6 & d.all$mark == "badsoft"], na.rm = T)
    result$time.b6.goodalco <- median(d.all$tick[d.all$block== 6 & d.all$mark == "goodalco"], na.rm = T)
    
    result$time.b7.badsoft  <- median(d.all$tick[d.all$block== 7 & d.all$mark == "badsoft"], na.rm = T)
    result$time.b7.goodalco <- median(d.all$tick[d.all$block== 7 & d.all$mark == "goodalco"], na.rm = T)
    
    # time ratios
    result$ratio.time.b1 <- median(d.all$tick[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)/median(d.all$tick[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    result$ratio.time.b2 <- median(d.all$tick[d.all$block== 2 & d.all$mark == "good"], na.rm = T)/median(d.all$tick[d.all$block== 2 & d.all$mark == "bad"], na.rm = T)
    result$ratio.time.b3 <- median(d.all$tick[d.all$block== 3 & d.all$mark == "goodsoft"], na.rm = T)/median(d.all$tick[d.all$block== 3 & d.all$mark == "badalco"], na.rm = T)
    result$ratio.time.b4 <- median(d.all$tick[d.all$block== 4 & d.all$mark == "goodsoft"], na.rm = T)/median(d.all$tick[d.all$block== 4 & d.all$mark == "badalco"], na.rm = T)
    result$ratio.time.b5 <- median(d.all$tick[d.all$block== 5 & d.all$mark == "bad"], na.rm = T)/median(d.all$tick[d.all$block== 5 & d.all$mark == "good"], na.rm = T)
    result$ratio.time.b6 <- median(d.all$tick[d.all$block== 6 & d.all$mark == "badsoft"], na.rm = T)/median(d.all$tick[d.all$block== 6 & d.all$mark == "goodalco"], na.rm = T)
    result$ratio.time.b7 <- median(d.all$tick[d.all$block== 7 & d.all$mark == "badsoft"], na.rm = T)/median(d.all$tick[d.all$block== 7 & d.all$mark == "goodalco"], na.rm = T)
    
    # time corrections
    result$corrected.time.b3.good <- median(d.all$tick[d.all$block== 3 & d.all$mark == "goodsoft"], na.rm = T) - median(d.all$tick[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)
    result$corrected.time.b3.soft <- median(d.all$tick[d.all$block== 3 & d.all$mark == "goodsoft"], na.rm = T) - median(d.all$tick[d.all$block== 2 & d.all$mark == "good"], na.rm = T)
    result$corrected.time.b3.bad  <- median(d.all$tick[d.all$block== 3 & d.all$mark == "badalco"], na.rm = T) - median(d.all$tick[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    result$corrected.time.b3.alco <- median(d.all$tick[d.all$block== 3 & d.all$mark == "badalco"], na.rm = T) - median(d.all$tick[d.all$block== 2 & d.all$mark == "bad"], na.rm = T)
    
    result$corrected.time.b4.good <- median(d.all$tick[d.all$block== 4 & d.all$mark == "goodsoft"], na.rm = T) - median(d.all$tick[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)
    result$corrected.time.b4.soft <- median(d.all$tick[d.all$block== 4 & d.all$mark == "goodsoft"], na.rm = T) - median(d.all$tick[d.all$block== 2 & d.all$mark == "good"], na.rm = T)
    result$corrected.time.b4.bad  <- median(d.all$tick[d.all$block== 4 & d.all$mark == "badalco"], na.rm = T) - median(d.all$tick[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    result$corrected.time.b4.alco <- median(d.all$tick[d.all$block== 4 & d.all$mark == "badalco"], na.rm = T) - median(d.all$tick[d.all$block== 2 & d.all$mark == "bad"], na.rm = T)
    
    result$corrected.time.b6.bad  <- median(d.all$tick[d.all$block== 6 & d.all$mark == "badsoft"], na.rm = T) - median(d.all$tick[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)
    result$corrected.time.b6.soft <- median(d.all$tick[d.all$block== 6 & d.all$mark == "badsoft"], na.rm = T) - median(d.all$tick[d.all$block== 5 & d.all$mark == "bad"], na.rm = T)
    result$corrected.time.b6.good <- median(d.all$tick[d.all$block== 6 & d.all$mark == "goodalco"], na.rm = T) - median(d.all$tick[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    result$corrected.time.b6.alco <- median(d.all$tick[d.all$block== 6 & d.all$mark == "goodalco"], na.rm = T) - median(d.all$tick[d.all$block== 5 & d.all$mark == "good"], na.rm = T)
    
    result$corrected.time.b7.bad  <- median(d.all$tick[d.all$block== 7 & d.all$mark == "badsoft"], na.rm = T) - median(d.all$tick[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)
    result$corrected.time.b7.soft <- median(d.all$tick[d.all$block== 7 & d.all$mark == "badsoft"], na.rm = T) - median(d.all$tick[d.all$block== 5 & d.all$mark == "bad"], na.rm = T)
    result$corrected.time.b7.good <- median(d.all$tick[d.all$block== 7 & d.all$mark == "goodalco"], na.rm = T) - median(d.all$tick[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    result$corrected.time.b7.alco <- median(d.all$tick[d.all$block== 7 & d.all$mark == "goodalco"], na.rm = T) - median(d.all$tick[d.all$block== 5 & d.all$mark == "good"], na.rm = T)
    
    # tap duration
    result$duration.b1.soft     <- median(d.all$duration[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)
    result$duration.b1.alco     <- median(d.all$duration[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    
    result$duration.b2.good     <- median(d.all$duration[d.all$block== 2 & d.all$mark == "good"], na.rm = T)
    result$duration.b2.bad      <- median(d.all$duration[d.all$block== 2 & d.all$mark == "bad"], na.rm = T)
    
    result$duration.b3.goodsoft <- median(d.all$duration[d.all$block== 3 & d.all$mark == "goodsoft"], na.rm = T)
    result$duration.b3.badalco  <- median(d.all$duration[d.all$block== 3 & d.all$mark == "badalco"], na.rm = T)
    
    result$duration.b4.goodsoft <- median(d.all$duration[d.all$block== 4 & d.all$mark == "goodsoft"], na.rm = T)
    result$duration.b4.badalco  <- median(d.all$duration[d.all$block== 4 & d.all$mark == "badalco"], na.rm = T)
    
    result$duration.b5.bad      <- median(d.all$duration[d.all$block== 5 & d.all$mark == "bad"], na.rm = T)
    result$duration.b5.good     <- median(d.all$duration[d.all$block== 5 & d.all$mark == "good"], na.rm = T)
    
    result$duration.b6.badsoft  <- median(d.all$duration[d.all$block== 6 & d.all$mark == "badsoft"], na.rm = T)
    result$duration.b6.goodalco <- median(d.all$duration[d.all$block== 6 & d.all$mark == "goodalco"], na.rm = T)
    
    result$duration.b7.badsoft  <- median(d.all$duration[d.all$block== 7 & d.all$mark == "badsoft"], na.rm = T)
    result$duration.b7.goodalco <- median(d.all$duration[d.all$block== 7 & d.all$mark == "goodalco"], na.rm = T)
    
    
    # duration ratios
    result$ratio.duration.b1 <- median(d.all$duration[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)/median(d.all$duration[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    result$ratio.duration.b2 <- median(d.all$duration[d.all$block== 2 & d.all$mark == "good"], na.rm = T)/median(d.all$duration[d.all$block== 2 & d.all$mark == "bad"], na.rm = T)
    result$ratio.duration.b3 <- median(d.all$duration[d.all$block== 3 & d.all$mark == "goodsoft"], na.rm = T)/median(d.all$duration[d.all$block== 3 & d.all$mark == "badalco"], na.rm = T)
    result$ratio.duration.b4 <- median(d.all$duration[d.all$block== 4 & d.all$mark == "goodsoft"], na.rm = T)/median(d.all$duration[d.all$block== 4 & d.all$mark == "badalco"], na.rm = T)
    result$ratio.duration.b5 <- median(d.all$duration[d.all$block== 5 & d.all$mark == "bad"], na.rm = T)/median(d.all$duration[d.all$block== 5 & d.all$mark == "good"], na.rm = T)
    result$ratio.duration.b6 <- median(d.all$duration[d.all$block== 6 & d.all$mark == "badsoft"], na.rm = T)/median(d.all$duration[d.all$block== 6 & d.all$mark == "goodalco"], na.rm = T)
    result$ratio.duration.b7 <- median(d.all$duration[d.all$block== 7 & d.all$mark == "badsoft"], na.rm = T)/median(d.all$duration[d.all$block== 7 & d.all$mark == "goodalco"], na.rm = T)
    
    # duration corrections
    result$corrected.duration.b3.good <- median(d.all$duration[d.all$block== 3 & d.all$mark == "goodsoft"], na.rm = T) - median(d.all$duration[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)
    result$corrected.duration.b3.soft <- median(d.all$duration[d.all$block== 3 & d.all$mark == "goodsoft"], na.rm = T) - median(d.all$duration[d.all$block== 2 & d.all$mark == "good"], na.rm = T)
    result$corrected.duration.b3.bad  <- median(d.all$duration[d.all$block== 3 & d.all$mark == "badalco"], na.rm = T) - median(d.all$duration[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    result$corrected.duration.b3.alco <- median(d.all$duration[d.all$block== 3 & d.all$mark == "badalco"], na.rm = T) - median(d.all$duration[d.all$block== 2 & d.all$mark == "bad"], na.rm = T)
    
    result$corrected.duration.b4.good <- median(d.all$duration[d.all$block== 4 & d.all$mark == "goodsoft"], na.rm = T) - median(d.all$duration[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)
    result$corrected.duration.b4.soft <- median(d.all$duration[d.all$block== 4 & d.all$mark == "goodsoft"], na.rm = T) - median(d.all$duration[d.all$block== 2 & d.all$mark == "good"], na.rm = T)
    result$corrected.duration.b4.bad  <- median(d.all$duration[d.all$block== 4 & d.all$mark == "badalco"], na.rm = T) - median(d.all$duration[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    result$corrected.duration.b4.alco <- median(d.all$duration[d.all$block== 4 & d.all$mark == "badalco"], na.rm = T) - median(d.all$duration[d.all$block== 2 & d.all$mark == "bad"], na.rm = T)
    
    result$corrected.duration.b6.bad  <- median(d.all$duration[d.all$block== 6 & d.all$mark == "badsoft"], na.rm = T) - median(d.all$duration[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)
    result$corrected.duration.b6.soft <- median(d.all$duration[d.all$block== 6 & d.all$mark == "badsoft"], na.rm = T) - median(d.all$duration[d.all$block== 5 & d.all$mark == "bad"], na.rm = T)
    result$corrected.duration.b6.good <- median(d.all$duration[d.all$block== 6 & d.all$mark == "goodalco"], na.rm = T) - median(d.all$duration[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    result$corrected.duration.b6.alco <- median(d.all$duration[d.all$block== 6 & d.all$mark == "goodalco"], na.rm = T) - median(d.all$duration[d.all$block== 5 & d.all$mark == "good"], na.rm = T)
    
    result$corrected.duration.b7.bad  <- median(d.all$duration[d.all$block== 7 & d.all$mark == "badsoft"], na.rm = T) - median(d.all$duration[d.all$block== 1 & d.all$mark == "soft"], na.rm = T)
    result$corrected.duration.b7.soft <- median(d.all$duration[d.all$block== 7 & d.all$mark == "badsoft"], na.rm = T) - median(d.all$duration[d.all$block== 5 & d.all$mark == "bad"], na.rm = T)
    result$corrected.duration.b7.good <- median(d.all$duration[d.all$block== 7 & d.all$mark == "goodalco"], na.rm = T) - median(d.all$duration[d.all$block== 1 & d.all$mark == "alco"], na.rm = T)
    result$corrected.duration.b7.alco <- median(d.all$duration[d.all$block== 7 & d.all$mark == "goodalco"], na.rm = T) - median(d.all$duration[d.all$block== 5 & d.all$mark == "good"], na.rm = T)
    
    ################################################################################
    # true results with filtered errors - ticks measured from correct answers ONLY
    d.true <- d[d$error == "false",]
    d.true$tick <- c(NA,diff(d.true$time))
    
    # data quality for true results ONLY - fast and slow answers
    result$true.fast.ans    <- sum(d.true$tick < 300, na.rm = T)
    result$true.slow.ans    <- sum(d.true$tick > 10000, na.rm = T)
    
    # TRUE time intervals
    result$time.true.b1.soft     <- median(d.true$tick[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)
    result$time.true.b1.alco     <- median(d.true$tick[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)

    result$time.true.b2.good     <- median(d.true$tick[d.true$block== 2 & d.true$mark == "good"], na.rm = T)
    result$time.true.b2.bad      <- median(d.true$tick[d.true$block== 2 & d.true$mark == "bad"], na.rm = T)
    
    result$time.true.b3.goodsoft <- median(d.true$tick[d.true$block== 3 & d.true$mark == "goodsoft"], na.rm = T)
    result$time.true.b3.badalco  <- median(d.true$tick[d.true$block== 3 & d.true$mark == "badalco"], na.rm = T)
    
    result$time.true.b4.goodsoft <- median(d.true$tick[d.true$block== 4 & d.true$mark == "goodsoft"], na.rm = T)
    result$time.true.b4.badalco  <- median(d.true$tick[d.true$block== 4 & d.true$mark == "badalco"], na.rm = T)
    
    result$time.true.b5.bad      <- median(d.true$tick[d.true$block== 5 & d.true$mark == "bad"], na.rm = T)
    result$time.true.b5.good     <- median(d.true$tick[d.true$block== 5 & d.true$mark == "good"], na.rm = T)
    
    result$time.true.b6.badsoft  <- median(d.true$tick[d.true$block== 6 & d.true$mark == "badsoft"], na.rm = T)
    result$time.true.b6.goodalco <- median(d.true$tick[d.true$block== 6 & d.true$mark == "goodalco"], na.rm = T)
    
    result$time.true.b7.badsoft  <- median(d.true$tick[d.true$block== 7 & d.true$mark == "badsoft"], na.rm = T)
    result$time.true.b7.goodalco <- median(d.true$tick[d.true$block== 7 & d.true$mark == "goodalco"], na.rm = T)
    
    # TRUE time ratios
    result$ratio.time.b1 <- median(d.true$tick[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)/median(d.true$tick[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    result$ratio.time.b2 <- median(d.true$tick[d.true$block== 2 & d.true$mark == "good"], na.rm = T)/median(d.true$tick[d.true$block== 2 & d.true$mark == "bad"], na.rm = T)
    result$ratio.time.b3 <- median(d.true$tick[d.true$block== 3 & d.true$mark == "goodsoft"], na.rm = T)/median(d.true$tick[d.true$block== 3 & d.true$mark == "badalco"], na.rm = T)
    result$ratio.time.b4 <- median(d.true$tick[d.true$block== 4 & d.true$mark == "goodsoft"], na.rm = T)/median(d.true$tick[d.true$block== 4 & d.true$mark == "badalco"], na.rm = T)
    result$ratio.time.b5 <- median(d.true$tick[d.true$block== 5 & d.true$mark == "bad"], na.rm = T)/median(d.true$tick[d.true$block== 5 & d.true$mark == "good"], na.rm = T)
    result$ratio.time.b6 <- median(d.true$tick[d.true$block== 6 & d.true$mark == "badsoft"], na.rm = T)/median(d.true$tick[d.true$block== 6 & d.true$mark == "goodalco"], na.rm = T)
    result$ratio.time.b7 <- median(d.true$tick[d.true$block== 7 & d.true$mark == "badsoft"], na.rm = T)/median(d.true$tick[d.true$block== 7 & d.true$mark == "goodalco"], na.rm = T)
    
    # TRUE time corrections
    result$corrected.time.b3.good <- median(d.true$tick[d.true$block== 3 & d.true$mark == "goodsoft"], na.rm = T) - median(d.true$tick[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)
    result$corrected.time.b3.soft <- median(d.true$tick[d.true$block== 3 & d.true$mark == "goodsoft"], na.rm = T) - median(d.true$tick[d.true$block== 2 & d.true$mark == "good"], na.rm = T)
    result$corrected.time.b3.bad  <- median(d.true$tick[d.true$block== 3 & d.true$mark == "badalco"], na.rm = T) - median(d.true$tick[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    result$corrected.time.b3.alco <- median(d.true$tick[d.true$block== 3 & d.true$mark == "badalco"], na.rm = T) - median(d.true$tick[d.true$block== 2 & d.true$mark == "bad"], na.rm = T)
    
    result$corrected.time.b4.good <- median(d.true$tick[d.true$block== 4 & d.true$mark == "goodsoft"], na.rm = T) - median(d.true$tick[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)
    result$corrected.time.b4.soft <- median(d.true$tick[d.true$block== 4 & d.true$mark == "goodsoft"], na.rm = T) - median(d.true$tick[d.true$block== 2 & d.true$mark == "good"], na.rm = T)
    result$corrected.time.b4.bad  <- median(d.true$tick[d.true$block== 4 & d.true$mark == "badalco"], na.rm = T) - median(d.true$tick[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    result$corrected.time.b4.alco <- median(d.true$tick[d.true$block== 4 & d.true$mark == "badalco"], na.rm = T) - median(d.true$tick[d.true$block== 2 & d.true$mark == "bad"], na.rm = T)
    
    result$corrected.time.b6.bad  <- median(d.true$tick[d.true$block== 6 & d.true$mark == "badsoft"], na.rm = T) - median(d.true$tick[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)
    result$corrected.time.b6.soft <- median(d.true$tick[d.true$block== 6 & d.true$mark == "badsoft"], na.rm = T) - median(d.true$tick[d.true$block== 5 & d.true$mark == "bad"], na.rm = T)
    result$corrected.time.b6.good <- median(d.true$tick[d.true$block== 6 & d.true$mark == "goodalco"], na.rm = T) - median(d.true$tick[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    result$corrected.time.b6.alco <- median(d.true$tick[d.true$block== 6 & d.true$mark == "goodalco"], na.rm = T) - median(d.true$tick[d.true$block== 5 & d.true$mark == "good"], na.rm = T)
    
    result$corrected.time.b7.bad  <- median(d.true$tick[d.true$block== 7 & d.true$mark == "badsoft"], na.rm = T) - median(d.true$tick[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)
    result$corrected.time.b7.soft <- median(d.true$tick[d.true$block== 7 & d.true$mark == "badsoft"], na.rm = T) - median(d.true$tick[d.true$block== 5 & d.true$mark == "bad"], na.rm = T)
    result$corrected.time.b7.good <- median(d.true$tick[d.true$block== 7 & d.true$mark == "goodalco"], na.rm = T) - median(d.true$tick[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    result$corrected.time.b7.alco <- median(d.true$tick[d.true$block== 7 & d.true$mark == "goodalco"], na.rm = T) - median(d.true$tick[d.true$block== 5 & d.true$mark == "good"], na.rm = T)
    
    # TRUE tap duration
    result$duration.true.b1.soft     <- median(d.true$duration[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)
    result$duration.true.b1.alco     <- median(d.true$duration[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    
    result$duration.true.b2.good     <- median(d.true$duration[d.true$block== 2 & d.true$mark == "good"], na.rm = T)
    result$duration.true.b2.bad      <- median(d.true$duration[d.true$block== 2 & d.true$mark == "bad"], na.rm = T)
    
    result$duration.true.b3.goodsoft <- median(d.true$duration[d.true$block== 3 & d.true$mark == "goodsoft"], na.rm = T)
    result$duration.true.b3.badalco  <- median(d.true$duration[d.true$block== 3 & d.true$mark == "badalco"], na.rm = T)
    
    result$duration.true.b4.goodsoft <- median(d.true$duration[d.true$block== 4 & d.true$mark == "goodsoft"], na.rm = T)
    result$duration.true.b4.badalco  <- median(d.true$duration[d.true$block== 4 & d.true$mark == "badalco"], na.rm = T)
    
    result$duration.true.b5.bad      <- median(d.true$duration[d.true$block== 5 & d.true$mark == "bad"], na.rm = T)
    result$duration.true.b5.good     <- median(d.true$duration[d.true$block== 5 & d.true$mark == "good"], na.rm = T)
    
    result$duration.true.b6.badsoft  <- median(d.true$duration[d.true$block== 6 & d.true$mark == "badsoft"], na.rm = T)
    result$duration.true.b6.goodalco <- median(d.true$duration[d.true$block== 6 & d.true$mark == "goodalco"], na.rm = T)
    
    result$duration.true.b7.badsoft  <- median(d.true$duration[d.true$block== 7 & d.true$mark == "badsoft"], na.rm = T)
    result$duration.true.b7.goodalco <- median(d.true$duration[d.true$block== 7 & d.true$mark == "goodalco"], na.rm = T)
    
    # TRUE duration ratios
    result$ratio.duration.b1 <- median(d.true$duration[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)/median(d.true$duration[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    result$ratio.duration.b2 <- median(d.true$duration[d.true$block== 2 & d.true$mark == "good"], na.rm = T)/median(d.true$duration[d.true$block== 2 & d.true$mark == "bad"], na.rm = T)
    result$ratio.duration.b3 <- median(d.true$duration[d.true$block== 3 & d.true$mark == "goodsoft"], na.rm = T)/median(d.true$duration[d.true$block== 3 & d.true$mark == "badalco"], na.rm = T)
    result$ratio.duration.b4 <- median(d.true$duration[d.true$block== 4 & d.true$mark == "goodsoft"], na.rm = T)/median(d.true$duration[d.true$block== 4 & d.true$mark == "badalco"], na.rm = T)
    result$ratio.duration.b5 <- median(d.true$duration[d.true$block== 5 & d.true$mark == "bad"], na.rm = T)/median(d.true$duration[d.true$block== 5 & d.true$mark == "good"], na.rm = T)
    result$ratio.duration.b6 <- median(d.true$duration[d.true$block== 6 & d.true$mark == "badsoft"], na.rm = T)/median(d.true$duration[d.true$block== 6 & d.true$mark == "goodalco"], na.rm = T)
    result$ratio.duration.b7 <- median(d.true$duration[d.true$block== 7 & d.true$mark == "badsoft"], na.rm = T)/median(d.true$duration[d.true$block== 7 & d.true$mark == "goodalco"], na.rm = T)
    
    # TRUE duration corrections
    result$corrected.duration.b3.good <- median(d.true$duration[d.true$block== 3 & d.true$mark == "goodsoft"], na.rm = T) - median(d.true$duration[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)
    result$corrected.duration.b3.soft <- median(d.true$duration[d.true$block== 3 & d.true$mark == "goodsoft"], na.rm = T) - median(d.true$duration[d.true$block== 2 & d.true$mark == "good"], na.rm = T)
    result$corrected.duration.b3.bad  <- median(d.true$duration[d.true$block== 3 & d.true$mark == "badalco"], na.rm = T) - median(d.true$duration[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    result$corrected.duration.b3.alco <- median(d.true$duration[d.true$block== 3 & d.true$mark == "badalco"], na.rm = T) - median(d.true$duration[d.true$block== 2 & d.true$mark == "bad"], na.rm = T)
    
    result$corrected.duration.b4.good <- median(d.true$duration[d.true$block== 4 & d.true$mark == "goodsoft"], na.rm = T) - median(d.true$duration[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)
    result$corrected.duration.b4.soft <- median(d.true$duration[d.true$block== 4 & d.true$mark == "goodsoft"], na.rm = T) - median(d.true$duration[d.true$block== 2 & d.true$mark == "good"], na.rm = T)
    result$corrected.duration.b4.bad  <- median(d.true$duration[d.true$block== 4 & d.true$mark == "badalco"], na.rm = T) - median(d.true$duration[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    result$corrected.duration.b4.alco <- median(d.true$duration[d.true$block== 4 & d.true$mark == "badalco"], na.rm = T) - median(d.true$duration[d.true$block== 2 & d.true$mark == "bad"], na.rm = T)
    
    result$corrected.duration.b6.bad  <- median(d.true$duration[d.true$block== 6 & d.true$mark == "badsoft"], na.rm = T) - median(d.true$duration[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)
    result$corrected.duration.b6.soft <- median(d.true$duration[d.true$block== 6 & d.true$mark == "badsoft"], na.rm = T) - median(d.true$duration[d.true$block== 5 & d.true$mark == "bad"], na.rm = T)
    result$corrected.duration.b6.good <- median(d.true$duration[d.true$block== 6 & d.true$mark == "goodalco"], na.rm = T) - median(d.true$duration[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    result$corrected.duration.b6.alco <- median(d.true$duration[d.true$block== 6 & d.true$mark == "goodalco"], na.rm = T) - median(d.true$duration[d.true$block== 5 & d.true$mark == "good"], na.rm = T)
    
    result$corrected.duration.b7.bad  <- median(d.true$duration[d.true$block== 7 & d.true$mark == "badsoft"], na.rm = T) - median(d.true$duration[d.true$block== 1 & d.true$mark == "soft"], na.rm = T)
    result$corrected.duration.b7.soft <- median(d.true$duration[d.true$block== 7 & d.true$mark == "badsoft"], na.rm = T) - median(d.true$duration[d.true$block== 5 & d.true$mark == "bad"], na.rm = T)
    result$corrected.duration.b7.good <- median(d.true$duration[d.true$block== 7 & d.true$mark == "goodalco"], na.rm = T) - median(d.true$duration[d.true$block== 1 & d.true$mark == "alco"], na.rm = T)
    result$corrected.duration.b7.alco <- median(d.true$duration[d.true$block== 7 & d.true$mark == "goodalco"], na.rm = T) - median(d.true$duration[d.true$block== 5 & d.true$mark == "good"], na.rm = T)
    
    
    
    d$time     <- as.integer(d$time/1000)
    d$datetime <- as.POSIXct(d$time, origin="1970-01-01")
    d$datetime <- format(d$datetime, format="%H:%M:%S")

    # Результаты
    
    tbl <- t(result)
    tbl <- cbind(names(result), tbl); rownames(tbl) <- NULL
    colnames(tbl) <- c("Показатель", "Численное значение")
    tbl <- knitr::kable(tbl, align = "c")
    kableExtra::kable_styling(tbl, "striped", full_width = T, position = "left")
    
    # save results
    if(file.exists(sprintf("%sreport.csv", gsub(basename(filename), "", tools::file_path_as_absolute(filename))))) {
      write.table(result, file = sprintf("%sreport.csv",gsub(basename(filename), "", tools::file_path_as_absolute(filename))),
                  sep = ";", row.names = F, append = T, col.names = F)
    } else {
      write.table(result, file = sprintf("%sreport.csv",gsub(basename(filename), "", tools::file_path_as_absolute(filename))), sep = ";", row.names = F)
    }
  }
}

# usage:
# do.estimation(filename = "result.csv")

