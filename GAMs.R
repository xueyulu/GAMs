rm(list=ls())
library(mgcv)
getwd()
setwd("D:/0_CODE_R_/GAM")

#import data
data <- read.table("data.txt", head= TRUE, sep="\t")
data <- as.matrix(data)
data

#extract data
#The response variable is Mean, while the explanatory variable is Depth
Depth<-data[,1]
Mean<-data[,2]

#run GAMs
result <- gam(Mean ~ s(Depth), method = "REML")
summary(result)
plot(result,se=T,resid=T,pch=17)
plot(result,se=F,resid=F,pch=17)

#Generate prediction values and confidence intervals
Depth_new <- seq(min(Depth), max(Depth), length.out = 200)
predictions <- predict(result, newdata = list(Depth = Depth_new), se.fit = TRUE)
ci_upper <- predictions$fit + 1.96 * predictions$se.fit
ci_lower <- predictions$fit - 1.96 * predictions$se.fit

# save GAMs data as txt
output <- data.frame(  
  Depth = Depth_new,  
  Prediction = predictions$fit,  
  CI_Upper = ci_upper,  
  CI_Lower = ci_lower  
)
write.table(output, "output.txt", row.names = FALSE, sep = "\t")  

