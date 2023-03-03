#STAT 452 HWK#5

#summary of foot data
df <- FOOTtxt
summary(FOOTtxt)

#Plot a scatter plot of the data. Does it suggest a 
#linear relationship between the shoe size and foot
#length?

fl <- df$Foot.Length
ss <- df$Shoe.Size

plot(fl, ss, xlab="foot length", ylab="shoe size", cex.lab=1.3, cex.axis=1.2)

#Fit a simple linear regression model to this data. 
lm(fl~ss)
intercept<-lm(fl~ss)$coefficients[1]
slope<-lm(fl~ss)$coefficients[2]
abline(intercept, slope, col = "red", lwd=2)

#shoe size 10 what foot length?
efl <- 15.520 + 1.042*10
efl

#Compute fitted values for all observations. What is the fitted value of foot 
#length for someone wearing size 10?

fit<-lm(fl~ss)
predict(lm(fl ~ ss), se.fit = FALSE)
fittedY<-predict(lm(fl ~ ss), se.fit = FALSE)

new<-data.frame(ss=10)
predict(fit, new)

#Compute residuals for all observations. What is the residual (or residuals) 
#for someone (everyone)wearing size 10? You will have several observations with 
#size 10. Please report the residuals for all of them

res<-y - predict(lm(fl ~ ss), se.fit = FALSE)
# we need to find the observed foot lendth at shoe size 10
obsrem<-fl[ss==10]
obsrem
#predictted value
predict(fit, new)
#residual
fl[ss==10]-predict(fit, new)