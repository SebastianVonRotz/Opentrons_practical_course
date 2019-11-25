library(ggplot2)

df=read.csv(file="BSA_PierceAssay_Calibration.csv",sep=";", dec=".", header=TRUE, stringsAsFactors=FALSE)
df=df[33:40, 3:5] # creates a subset of the file
df$X.3=as.numeric(df$X.3)
df$conc = c(1000, 500, 250, 125, 62.5, 31.25, 15.625, 0) # Add theoretical concetrations
rownames(df) <- c("1000","500","250","125","62.5","31.25","15.625", "Blanc")
colnames(df) <- c("M1","M2","M3", "conc")
df$mean=0 # create new column
for(i in 1:8) {
  df$mean[i] = rowMeans(df[i,1:3])# iterate trough column and add mean
}
df$mean=df$mean-df$mean[8] #subtracting the blank from every mean entry

lm(formula = df$mean ~ df$conc)
m <- lm(df$conc ~ df$mean)
a <- signif(coef(m)[1], digits = 5)
b <- signif(coef(m)[2], digits = 5)
s=summary(m)
r <- signif(s$r.squared, digits = 5)
textlab <- paste("y = ",b,"x + ",a," R^2= ",r, sep="")
print(textlab)

ggplot(data = df, aes(x = conc, y = mean)) + geom_point(color="red") + geom_smooth(method = "lm", se = F) +
  geom_text(aes(x = 250, y = 1.5, label = textlab), color="black", size=5, parse = FALSE)


