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

res<-fl - predict(lm(fl ~ ss), se.fit = FALSE)
# we need to find the observed foot lendth at shoe size 10
obsrem<-fl[ss==10]
obsrem
#predictted value
predict(fit, new)
#residual
fl[ss==10]-predict(fit, new)

#Estimate sigma

#fit<-lm(fl ~ ss)
shat<-sum(res^2) 
n<-length(res)
shat<-shat/(n-2)
#estimate of sigma^2
shat
#estimate of s^hat
sqrt(shat)

#Find sum of residuals
sum(res)

#What is the probability that a person who wears size 10 will have the foot 
#length smaller that 24cm?

#We need the mean and standard deviation of T|x=15
#to find the mean= fitted value of removal efficiency when shoe size=10
new1<-data.frame(x=10)
m<-predict.lm(fit, new1)
m

#the standard deviation is s^hat (1.0606)
#The distribution of Y|x=10 is normal with mean m and st. dev=s^hat = 1.0606
# now we compute the required probability:
1 - pnorm(24, mean=m, sd=1.0606)

# Find the Pearson correlation coefficient between the shoe size and foot length. 
cor(ss,fl)

#Check if the model assumptions are reasonably satisfied for this data. 
#For the report, use only standardized residuals. 

#First, we standardize the residuals (subtract the mean and divide by st.dev)
mr<-mean(res)
stdevr<-sqrt(var(res))
st.res<-(res-mr)/stdevr
#now we plot
par(mfrow=c(2,2))
plot(ss, res, main="foot length vs. shoe size")
plot(fittedY, res, main="Residuals vs. Fitted Values")
hist(res, main="Histogram of the Residuals")
qqnorm(res)

