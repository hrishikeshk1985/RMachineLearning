library(vcd)
data("Trucks")
head(Trucks)
View(Trucks)
Trucks.wd<- Trucks[rep(1:nrow(Trucks),Trucks$Freq),]
Trucks.wd
Trucks.rm = Trucks.wd[, -(c(1,5))]
set.seed(456)
Trucks.sample = Trucks.rm[sample(nrow(Trucks.rm), 100), ]
Trucks.onoff = data.frame(matrix(nrow =nrow(Trucks.sample), ncol = ncol (Trucks.sample)))
Trucks.onoff
for (i in 1:nrow(Trucks.sample)) {
   for (j in 1:ncol(Trucks.sample)) {
    if (Trucks.sample[i,j] != Trucks.sample[1,j])
      Trucks.onoff[i,j] = 0
      else Trucks.onoff[i,j] = 1
      }
}
names(Trucks.onoff)=names(Trucks.sample)
Trucks.onoff
b = hclust(dist(Trucks.onoff, method= "binary"))
plot(b)
Trucks.onoff[c(96,92,76,52,5,23),]
