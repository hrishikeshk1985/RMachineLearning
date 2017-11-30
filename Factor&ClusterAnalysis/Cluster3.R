library(HDclassif)
data("wine")
str(wine)
names(wine)=c("Class", "Alcohol", "MalicAcid","Ash","Alk_ash","magnesium","T_phenols","Flavenoids","Non_flav","Proantho","C_Intensity","Hue","OD280","Proline")
names(wine)
df$class=as.factor(wine$Class)
df$class
