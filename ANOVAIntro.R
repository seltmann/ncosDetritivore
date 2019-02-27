
#set as your working directory
setwd("~/Documents/ncosDetritivore")

#read in your csv file, you can give it the same name or a new name
nipomo2017 <- read.csv("data/nipomolupine2017.csv")

#up to you, I generally remove all NAs with code below
# remove blank rows
nipomo2017 <- nipomo2017[!apply(is.na(nipomo2017) | nipomo2017 == "", 1, all),] 

#look at the dataset
head(nipomo2017)

#this loads in packages that you may want to use (I use them for almost every code)
#if the code doesn't work you click tools, install packages then type in the names
library(ggplot2)
library(plyr)

#use summary(aov()) in order to run an ANOVA and get stats value.  
#An ANOVA
summary(aov(seeds ~ cage,data=nipomo2017))

#A two-way ANOVA informs you whether the effect of one of your independent variables on the dependent variable is the same for all values of your other independent variable. In this example, is effect of cage type on seed set influenced by landscape topography?

#The two-way ANOVA compares the mean differences between groups that have been split on two independent variables (called factors). The primary purpose of a two-way ANOVA is to understand if there is an interaction between the two independent variables (cage and Topo) on the dependent variable (seeds). If it was caged or not, did that affect Topo? Dependant variables are not the ones you are varying.

summary(aov(seeds ~ cage+Topo,data=nipomo2017))

#use TukeyHSD to do post-poc analysis to tell you were the significance pairing is
#When writing results you need the p value and the F value
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.
#adjusted p-value < 0.05 = standard significant with only 5% chance that the differences are random or reject hypothesis with a 95% probability.
TukeyHSD(aov(seeds~cage+Topo,data=nipomo2017))


####-----------######
#use the ddply function to separate data out as necessary to visualize data
#here I used ddply to pull out all data with associated with Topography (all same topographies)
#or identical fields within the column will be consolidated, what you do with the consolidated data varies
#I averaged the seed production for different topographies 
#Calculated the standard error (use this not deviation) which is the SD/sqrt(n)
Seeds_Topo <- ddply(nipomo2017, c("Topo"),summarise,
                    avgseed = mean(seeds),
                    error = sd(seeds)/(sqrt(length(seeds)))) #why are some error 

#basic bar plot
plot(x=nipomo2017$Topo,y=nipomo2017$seeds)

#basic bar ggplots, which look better than the plot function and have more customization
#is coded as such
ggplot(aes(x=Topo,y=seeds,fill=Topo,color=Topo),
       data=nipomo2017) + geom_boxplot()

# Put it into a separate dataframe in order to create panel graphs
######---not working

SeedsGraph_Topo <- ggplot(aes(x=Topo,y=seeds,fill=Topo,color=Topo),
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

