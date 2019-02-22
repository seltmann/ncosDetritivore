
#first read in your csv file, you can give it the same name or a new name

nipomo2017 <- read.csv("C:/Users/Justin Luong/Desktop/Stats/csv/nipomolupine2017.csv")

#up to you, I generally remove all NAs with code below
nipomo2017 <- nipomo2017[!apply(is.na(nipomo2017) | nipomo2017 == "", 1, all),] # remove blank rows

#this loads in packages that you may want to use (I use them for almost every code)
#if the code doesn't work you click tools, install packages then type in the names
library(ggplot2)
library(plyr)

#use summary(aov()) in order to run ANOVA and get stats value
summary(aov(seeds~cage+Topo,data=nipomo2017))

#use TukeyHSD to do post-poc analysis to tell you were the significance pairing is
#When writing results you need the p value and the F value
TukeyHSD(aov(seeds~Cage+Topography,data=yearlydata))

# use the ddply function to separate data out as necessary to visualize data
#here I used ddply to pull out all data with associated with Topography (all same topographies)
#or identical fields within the column will be consolidated, what you do with the consolidated data varies
#for this case I averaged the seed production for different topographies then 
#I calculated the standard error (use this not deviation) which is the SD/sqrt(n)
Seeds_Topo <- ddply(yearlydata, c("Topography"),summarise,
                    avgseed = mean(seeds),
                    error = sd(seeds)/(sqrt(length(seeds))))

#basic bar ggplots, which look better than the plot function and have more customization
#is coded as such:

ggplot(aes(x=Topography,y=avgseed,fill=Topography,color=Topography),
       data=Seeds_Topo)

# I generally put it into a database name in order to panel graphs
SeedsGraph_Topo <- ggplot(aes(x=Topography,y=avgseed,fill=Topography,color=Topography),
                          data=Seeds_Topo)+
  geom_bar(stat="identity",position="dodge",color="black",show.legend=FALSE)


#below is the graph with error bars added
SeedsGraph_Topo <- ggplot(aes(x=Topography,y=avgseed,fill=Topography,color=Topography),
                          data=Seeds_Topo)+
  geom_bar(stat="identity",position="dodge",color="black",show.legend=FALSE)
+geom_errorbar(aes(ymax=avgseed+error, ymin=avgseed-error),
               position="dodge",color="black",width=0.9) 

#below is more custom options I set:
SeedsGraph_Topo <- ggplot(aes(x=Topography,y=avgseed,fill=Topography,color=Topography),
                          data=Seeds_Topo)+
  geom_bar(stat="identity",position="dodge",color="black",show.legend=FALSE)
+geom_errorbar(aes(ymax=avgseed+error, ymin=avgseed-error),
               position="dodge",color="black",width=0.9) 
+ ylab("Avg Seed Pod Production")+xlab("Topography")
+theme(panel.border=element_rect(color="black",size=1,fill=NA))

