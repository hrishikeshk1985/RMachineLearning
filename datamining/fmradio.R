lastfm<-read.csv(file.choose())
View(lastfm)
lastfm$user<-factor(lastfm$user)
levels(lastfm$user)
levels(lastfm$artist)


library(arules)

playlist<-split(x=lastfm[,"artist"],f=lastfm$user)
playlist
playlist[1:2]

#remove duplicates
playlist<-lapply(playlist,unique)
#convert to transactions

playlist<-as(playlist,"transactions")
itemFrequency(playlist)
itemFrequencyPlot(playlist)
musicrules <- apriori(playlist,parameter=list(support=.01,confidence=.5))
inspect(musicrules)
inspect(subset(musicrules, subset=lift > 5))
inspect(sort(subset(musicrules,subset=lift>5),by="confidence"))
