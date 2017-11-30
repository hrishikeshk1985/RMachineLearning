ibra# bar plots
View(mtcars)
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution",xlab="Number of Gears")

counts1 <- table(mtcars$gear)
counts1
barplot(counts1, main="Car Distribution", horiz=TRUE,names.arg=c("3 Gears", "4 Gears", "5 Gears"))


# Multiple bar plots
counts2 <- table(mtcars$vs, mtcars$gear)
counts2
barplot(counts2, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

# Pie charts
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main="Pie Chart of Countries")


slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Pie Chart of Countries")

# histograms

hist(mtcars$mpg, breaks=12, col="red")

## Kernel Density Plot
d <- density(mtcars$mpg) # returns the density data
d
plot(d) 

# Boxplots
median(mtcars$mpg)
boxplot(mtcars$mpg)
summary(mtcars$qsec)
boxplot(mtcars$qsec)



#models grouped for a cylinder dot chart
x<-mtcars[order(mtcars$mpg),]
x$cyl<-factor(x$cyl)
x$color[x$cyl==4]<-"red"
x$color[x$cyl==6]<-"blue"
x$color[x$cyl==8]<-"darkgreen"
dotchart(x$mpg,labels=row.names(x),cex=1,groups=x$cyl,main="models grouped for a cylinder",xlab="Miles per gallon",color=x$color)





