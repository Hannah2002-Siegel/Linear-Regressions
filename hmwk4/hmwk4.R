#Hannah Siegel
#Homework 4



#lab dataframe (df)
yf <- cor_lab_data

#part 1 --> yf only (lab data)
# Compute all three correlation coefficients between x and y

y <- yf$y
x <- yf$x

#pearson
pc<-round(cor(x, y, method="pearson"), 2)
cat("Pearson correlation=", pc, "\n")

#spearman
sc<-round(cor(x, y, method="spearman"), 2)
cat("Spearman correlation=", sc, "\n")

#kendall
kc<-round(cor(x, y, method="kendall"), 2)
cat("Kendall correlation=", kc, "\n")

#remove outlier and recalculate
yf <- cor_lab_data[-c(10),]
y <- yf$y
x <- yf$x
show(yf)

#pearson
pc<-round(cor(x, y, method="pearson"), 2)
cat("Pearson correlation=", pc, "\n")

#spearman
sc<-round(cor(x, y, method="spearman"), 2)
cat("Spearman correlation=", sc, "\n")

#kendall
kc<-round(cor(x, y, method="kendall"), 2)
cat("Kendall correlation=", kc, "\n")



#part 2

#homework dataframe (xf)
xf <- cor_hwk_data

y <- xf$GPA
x <- xf$ACT

#Use an appropriate graph to make initial 
#assessment of the association between GPA
#and ACT scores.
plot(x,y)


#Test if the Pearson corr. coefficient between 
#ACT and GPA is significantly different from zero
cor.test(x,y, method="pearson")$p.value
cor.test(x,y, method="pearson")

#Test if the Spearman or Kendall (select one and use) 
#corr. coefficient between ACT and
#GPA is significantly different from zero.
cor.test(x,y, method="kendall")$p.value
cor.test(x,y, method="kendall")


