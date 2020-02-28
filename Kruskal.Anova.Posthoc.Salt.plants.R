#####Analyze Leif's salt accumulating plant data
#Noah Strom
#February 12, 2020

#Read in Leaching data.

non.leaching.data <- read.csv('~/Dropbox/Bo Hu/Noah/Leif/Non.leaching.data.2020.2.12.csv')

##Focus on ABOVE.Oven.g weights, as these will most likely be reliable.
##ROOT.Oven.g is next most reliable but is not completely reliable.  Fresh weights are not that reliable.




#Subset by plant.
non.leaching.data.Turf <- subset(non.leaching.data, Plant == 'Turf')
non.leaching.data.Barley <- subset(non.leaching.data, Plant == 'Barley')
non.leaching.data.SugarBeet <- subset(non.leaching.data, Plant == 'Sugar Beet')
non.leaching.data.Alfalfa <- subset(non.leaching.data, Plant == 'Alfalfa')
non.leaching.data.S.Europaea <- subset(non.leaching.data, Plant == 'S. Europaea')


#Turf
non.leaching.data.Turf


#Check assumptions of ANOVA:  homogenity of variance.  normally distributed data.
library(ggplot2)

ggplot(data=non.leaching.data.Turf, aes(non.leaching.data.Turf$ABOVE.Oven.g.)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~Treatment) +
  xlab('Above-ground oven-dried weight (g)') +
  ggtitle('Non-leaching Turf -- histograms')

ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/non.leaching.Turf.histogram.pdf', width = 5, height = 4)

library(car)
leveneTest(non.leaching.data.Turf$ABOVE.Oven.g. ~ non.leaching.data.Turf$Treatment)

#Variance homogeneous.  Try ANOVA
aov.out <- aov(non.leaching.data.Turf$ABOVE.Oven.g. ~ non.leaching.data.Turf$Treatment)
anova(aov.out)

#Also use Kruskal-Wallis test.
kruskal.test(non.leaching.data.Turf$ABOVE.Oven.g. ~ non.leaching.data.Turf$Treatment)

#Scatter plot (points by groups with Tukey's letters)
g.scatter.Turf <- ggplot(non.leaching.data.Turf, aes(x = Treatment, y = as.numeric(as.vector(non.leaching.data.Turf$ABOVE.Oven.g.)))) +
  geom_point() +
  ggtitle('Non-leaching Turf') +
  ylab('Above-ground oven-dried mass (g)')

#Save a plot
ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/Non.leaching.Turf.scatterplot.Tukey.letters.pdf', width = 4, height = 3)








#Barley
non.leaching.data.Barley

#Check assumptions of ANOVA:  homogenity of variance.  normally distributed data.
library(ggplot2)

ggplot(data=non.leaching.data.Barley, aes(non.leaching.data.Barley$ABOVE.Oven.g.)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~Treatment) +
  xlab('Above-ground oven-dried weight (g)') +
  ggtitle('Non-leaching Barley -- histograms')

ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/non.leaching.Barley.histogram.pdf', width = 5, height = 4)

library(car)
leveneTest(non.leaching.data.Barley$ABOVE.Oven.g. ~ non.leaching.data.Barley$Treatment)

#Variance homogeneous.  Try ANOVA
aov.out <- aov(non.leaching.data.Barley$ABOVE.Oven.g. ~ non.leaching.data.Barley$Treatment)
anova(aov.out) #Significant!

#Do a post-hoc test to see which groups are different.
HSD.out <- HSD.test(y = aov.out, trt = "non.leaching.data.Barley$Treatment")
Barley.HSD.data <- HSD.out$groups[c('Control', '50mM', '100mM', '200mM'),]
names(Barley.HSD.data) <- c('Above-ground oven-dried weight', 'groups')
Barley.HSD.data

#Also use Kruskal-Wallis test.
kruskal.test(non.leaching.data.Barley$ABOVE.Oven.g. ~ non.leaching.data.Barley$Treatment) #Significant!
#Post-hoc test for Kruskal-Wallis test.
library(dunn.test)
dunn.out <- dunn.test(x = non.leaching.data.Barley$ABOVE.Oven.g., g = non.leaching.data.Barley$Treatment)
library(multcomp)
#cld(dunn.out)
#install.packages('PMCMR')
library(PMCMR)
#non.leaching.data.Barley$Treatment <- factor(non.leaching.data.Barley$Treatment, levels = c('Control', '50mM', '100mM', '200mM'))
#dunn.test.control(x = non.leaching.data.Barley$ABOVE.Oven.g., g = non.leaching.data.Barley$Treatment, p.adj = 'none')

kruskal.out <- kruskal(y = non.leaching.data.Barley$ABOVE.Oven.g., trt = non.leaching.data.Barley$Treatment, p.adj = 'fdr')
kruskal.out$groups[c('Control', '50mM', '100mM', '200mM'),]

#Put treatments into correct order for plotting.
non.leaching.data.Barley$Treatment <- factor(non.leaching.data.Barley$Treatment, levels = c('Control', '50mM', '100mM', '200mM'))

#merge Tukey's letters with rest of data.
library(plyr)
Tukey.tops <- ddply(non.leaching.data.Barley, 'Treatment', function (x) max(fivenum(x$ABOVE.Oven.g.)))
Tukey.tops$Treatment <- factor(Tukey.tops$Treatment, levels = c('Control', '50mM', '100mM', '200mM'))
Tukey.tops <- Tukey.tops[order(Tukey.tops$Treatment),]
Tukey.tops$letters <- Barley.HSD.data$groups

#merge Kruskal's letters with rest of data.
library(plyr)
Kruskal.tops <- ddply(non.leaching.data.Barley, 'Treatment', function (x) max(fivenum(x$ABOVE.Oven.g.)))
Kruskal.tops$Treatment <- factor(Kruskal.tops$Treatment, levels = c('Control', '50mM', '100mM', '200mM'))
Kruskal.tops <- Kruskal.tops[order(Kruskal.tops$Treatment),]
Kruskal.tops$letters <- kruskal.out$groups[c('Control', '50mM', '100mM', '200mM'), 'groups']

#Scatter plot (points by groups with Tukey's letters)
ggplot(non.leaching.data.Barley, aes(x = Treatment, y = as.numeric(as.vector(non.leaching.data.Barley$ABOVE.Oven.g.)))) +
  geom_point() +
  #add tukey's letters in the correct location
  annotate(geom = 'text', x = Tukey.tops$Treatment, y = Tukey.tops$V1 + 1, label = Tukey.tops$letters, vjust = 0.5) +
  annotate(geom = 'text', x = Inf, y = Inf, label = 'P = 0.00018', hjust = 1, vjust = 1) +
  ggtitle('Non-leaching Barley') +
  ylab('Above-ground oven-dried mass (g)')

#Save a plot
ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/Non.leaching.Barley.scatterplot.Tukey.letters.pdf', width = 4, height = 3)

#Scatter plot (points by groups)
g.scatter.Barley <- ggplot(non.leaching.data.Barley, aes(x = Treatment, y = as.numeric(as.vector(non.leaching.data.Barley$ABOVE.Oven.g.)))) +
  geom_point() +
  ggtitle('Non-leaching Barley') +
  ylab('Above-ground oven-dried mass (g)') +
  annotate(geom = 'text', x = Inf, y = Inf, label = 'P = 0.00018', hjust = 1, vjust = 1)

g.scatter.Barley

#Save a plot
ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/Non.leaching.Barley.scatterplot.pdf', width = 4, height = 3)



#Scatter plot (points by groups with Kruskal's letters)
g.scatter.Barley <- ggplot(non.leaching.data.Barley, aes(x = Treatment, y = as.numeric(as.vector(non.leaching.data.Barley$ABOVE.Oven.g.)))) +
  geom_point() +
  ggtitle('Non-leaching Barley') +
  ylab('Above-ground oven-dried mass (g)') +
  annotate(geom = 'text', x = Inf, y = Inf, label = 'P = 0.025', hjust = 1, vjust = 1) +
  #add Kruskal's letters in the correct location
  annotate(geom = 'text', x = Kruskal.tops$Treatment, y = Kruskal.tops$V1 + 1, label = Kruskal.tops$letters, vjust = 0.5)

g.scatter.Barley

#Save a plot
ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/Non.leaching.Barley.scatterplot.Kruskal.pdf', width = 4, height = 3)






#SugarBeet
#Check assumptions of ANOVA:  homogenity of variance.  normally distributed data.
non.leaching.data.SugarBeet

library(ggplot2)

ggplot(data=non.leaching.data.SugarBeet, aes(non.leaching.data.SugarBeet$ABOVE.Oven.g.)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~Treatment) +
  xlab('Above-ground oven-dried weight (g)') +
  ggtitle('Non-leaching Sugar Beet -- histograms')

ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/non.leaching.SugarBeet.histogram.pdf', width = 5, height = 4)

library(car)
leveneTest(non.leaching.data.SugarBeet$ABOVE.Oven.g. ~ non.leaching.data.SugarBeet$Treatment)

#Variance homogeneous.  Try ANOVA
aov.out <- aov(non.leaching.data.SugarBeet$ABOVE.Oven.g. ~ non.leaching.data.SugarBeet$Treatment)
anova(aov.out) #Not significant!

#Do a post-hoc test to see which groups are different.
HSD.out <- HSD.test(y = aov.out, trt = "non.leaching.data.SugarBeet$Treatment")
SugarBeet.HSD.data <- HSD.out$groups[c('Control', '50mM', '100mM', '200mM'),]
names(SugarBeet.HSD.data) <- c('Above-ground oven-dried weight', 'groups')
SugarBeet.HSD.data

#Also use Kruskal-Wallis test.
kruskal.test(non.leaching.data.SugarBeet$ABOVE.Oven.g. ~ non.leaching.data.SugarBeet$Treatment) #Significant!
#Post-hoc test for Kruskal-Wallis test.
library(dunn.test)
dunn.out <- dunn.test(x = non.leaching.data.SugarBeet$ABOVE.Oven.g., g = non.leaching.data.SugarBeet$Treatment)
library(multcomp)
#cld(dunn.out)
#install.packages('PMCMR')
library(PMCMR)
#non.leaching.data.SugarBeet$Treatment <- factor(non.leaching.data.SugarBeet$Treatment, levels = c('Control', '50mM', '100mM', '200mM'))
#dunn.test.control(x = non.leaching.data.SugarBeet$ABOVE.Oven.g., g = non.leaching.data.SugarBeet$Treatment, p.adj = 'none')

kruskal.out <- kruskal(y = non.leaching.data.SugarBeet$ABOVE.Oven.g., trt = non.leaching.data.SugarBeet$Treatment, p.adj = 'fdr')
kruskal.out$groups[c('Control', '50mM', '100mM', '200mM'),]

#Scatter plot (points by groups with Tukey's letters)
g.scatter.SugarBeet <- ggplot(non.leaching.data.SugarBeet, aes(x = Treatment, y = as.numeric(as.vector(non.leaching.data.SugarBeet$ABOVE.Oven.g.)))) +
  geom_point() +
  ggtitle('Non-leaching SugarBeet') +
  ylab('Above-ground oven-dried mass (g)')

#Save a plot
ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/Non.leaching.SugarBeet.scatterplot.Tukey.letters.pdf', width = 4, height = 3)








#Alfalfa
non.leaching.data.Alfalfa

#Check assumptions of ANOVA:  homogenity of variance.  normally distributed data.

library(ggplot2)

ggplot(data=non.leaching.data.Alfalfa, aes(non.leaching.data.Alfalfa$ABOVE.Oven.g.)) + 
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~Treatment) +
  xlab('Above-ground oven-dried weight (g)') +
  ggtitle('Non-leaching Alfalfa -- histograms')

ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/non.leaching.Alfalfa.histogram.pdf', width = 5, height = 4)

library(car)
leveneTest(non.leaching.data.Alfalfa$ABOVE.Oven.g. ~ non.leaching.data.Alfalfa$Treatment)

#Variance homogeneous.  Try ANOVA
aov.out <- aov(non.leaching.data.Alfalfa$ABOVE.Oven.g. ~ non.leaching.data.Alfalfa$Treatment)
anova(aov.out) #Not significant!

#Do a post-hoc test to see which groups are different.
HSD.out <- HSD.test(y = aov.out, trt = "non.leaching.data.Alfalfa$Treatment")
Alfalfa.HSD.data <- HSD.out$groups[c('Control', '50mM', '100mM', '200mM'),]
names(Alfalfa.HSD.data) <- c('Above-ground oven-dried weight', 'groups')
Alfalfa.HSD.data

#Also use Kruskal-Wallis test.
kruskal.test(non.leaching.data.Alfalfa$ABOVE.Oven.g. ~ non.leaching.data.Alfalfa$Treatment) #Significant!
#Post-hoc test for Kruskal-Wallis test.
library(dunn.test)
dunn.out <- dunn.test(x = non.leaching.data.Alfalfa$ABOVE.Oven.g., g = non.leaching.data.Alfalfa$Treatment)
library(multcomp)
#cld(dunn.out)
#install.packages('PMCMR')
library(PMCMR)
#non.leaching.data.Alfalfa$Treatment <- factor(non.leaching.data.Alfalfa$Treatment, levels = c('Control', '50mM', '100mM', '200mM'))
#dunn.test.control(x = non.leaching.data.Alfalfa$ABOVE.Oven.g., g = non.leaching.data.Alfalfa$Treatment, p.adj = 'none')

kruskal.out <- kruskal(y = non.leaching.data.Alfalfa$ABOVE.Oven.g., trt = non.leaching.data.Alfalfa$Treatment, p.adj = 'fdr')
kruskal.out$groups[c('Control', '50mM', '100mM', '200mM'),]

#Scatter plot (points by groups with Tukey's letters)
g.scatter.Alfalfa <- ggplot(non.leaching.data.Alfalfa, aes(x = Treatment, y = as.numeric(as.vector(non.leaching.data.Alfalfa$ABOVE.Oven.g.)))) +
  geom_point() +
  ggtitle('Non-leaching Alfalfa') +
  ylab('Above-ground oven-dried mass (g)')

#Save a plot
ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/Non.leaching.Alfalfa.scatterplot.Tukey.letters.pdf', width = 4, height = 3)





#S.Europaea
non.leaching.data.S.Europaea

#Check assumptions of ANOVA:  homogenity of variance.  normally distributed data.
library(ggplot2)

ggplot(data=non.leaching.data.S.Europaea, aes(non.leaching.data.S.Europaea$ABOVE.Oven.g.)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~Treatment) +
  xlab('Above-ground oven-dried weight (g)') +
  ggtitle('Non-leaching S.Europaea -- histograms')

ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/non.leaching.S.Europaea.histogram.pdf', width = 5, height = 3)

library(car)
leveneTest(non.leaching.data.S.Europaea$ABOVE.Oven.g. ~ non.leaching.data.S.Europaea$Treatment)

#Variance homogeneous.  Try ANOVA
aov.out <- aov(non.leaching.data.S.Europaea$ABOVE.Oven.g. ~ non.leaching.data.S.Europaea$Treatment)
anova(aov.out) #Significant!


#Also use Kruskal-Wallis test.
kruskal.test(non.leaching.data.S.Europaea$ABOVE.Oven.g. ~ non.leaching.data.S.Europaea$Treatment) #Significant!


#Put treatments into correct order for plotting.
non.leaching.data.S.Europaea$Treatment <- factor(non.leaching.data.S.Europaea$Treatment, levels = c('Control', '250mM'))


#Scatter plot (points by groups with Tukey's letters)
g.scatter.S.Europaea <- ggplot(non.leaching.data.S.Europaea, aes(x = Treatment, y = as.numeric(as.vector(non.leaching.data.S.Europaea$ABOVE.Oven.g.)))) +
  geom_point() +
  #add tukey's letters in the correct location
  #annotate(geom = 'text', x = Tukey.tops$Treatment, y = Tukey.tops$V1 + 1, label = Tukey.tops$letters, vjust = 0.5) +
  annotate(geom = 'text', x = Inf, y = Inf, label = 'P = 0.0010', hjust = 1, vjust = 1) +
  ggtitle('Non-leaching S.Europaea') +
  ylab('Above-ground oven-dried mass (g)')

#Save a plot
ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/Non.leaching.S.Europaea.scatterplot.Tukey.letters.pdf', width = 4, height = 3)





################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Leaching data!

#Read in Leaching data.

leaching.data <- read.csv('~/Dropbox/Bo Hu/Noah/Leif/Leached.data.2020.2.12.csv')

##Focus on ABOVE.Oven.g weights, as these will most likely be reliable.
##ROOT.Oven.g is next most reliable but is not completely reliable.  Fresh weights are not that reliable.




#Subset by plant.
leaching.data.Turf <- subset(leaching.data, Plant == 'Turf')
leaching.data.Barley <- subset(leaching.data, Plant == 'Barley')
leaching.data.SugarBeet <- subset(leaching.data, Plant == 'Sugar Beet')
leaching.data.Alfalfa <- subset(leaching.data, Plant == 'Alfalfa')
leaching.data.S.Europaea <- subset(leaching.data, Plant == 'S. Europaea')


#Turf
leaching.data.Turf


#Check assumptions of ANOVA:  homogenity of variance.  normally distributed data.
library(ggplot2)

ggplot(data=leaching.data.Turf, aes(leaching.data.Turf$ABOVE.Oven.g.)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~Treatment) +
  xlab('Above-ground oven-dried weight (g)') +
  ggtitle('Leaching Turf -- histograms')

ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/leaching.Turf.histogram.pdf', width = 5, height = 4)

library(car)
leveneTest(leaching.data.Turf$ABOVE.Oven.g. ~ leaching.data.Turf$Treatment)

#Variance homogeneous.  Try ANOVA
aov.out <- aov(leaching.data.Turf$ABOVE.Oven.g. ~ leaching.data.Turf$Treatment)
anova(aov.out) #not significant.

#Also use Kruskal-Wallis test.
kruskal.test(leaching.data.Turf$ABOVE.Oven.g. ~ leaching.data.Turf$Treatment)




#Barley
leaching.data.Barley

#Check assumptions of ANOVA:  homogenity of variance.  normally distributed data.
library(ggplot2)

ggplot(data=leaching.data.Barley, aes(leaching.data.Barley$ABOVE.Oven.g.)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~Treatment) +
  xlab('Above-ground oven-dried weight (g)') +
  ggtitle('Leaching Barley -- histograms')

ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/leaching.Barley.histogram.pdf', width = 5, height = 4)

library(car)
leveneTest(leaching.data.Barley$ABOVE.Oven.g. ~ leaching.data.Barley$Treatment)

#Variance homogeneous.  Try ANOVA
aov.out <- aov(leaching.data.Barley$ABOVE.Oven.g. ~ leaching.data.Barley$Treatment)
anova(aov.out) #Not Significant!

#Do a post-hoc test to see which groups are different.
HSD.out <- HSD.test(y = aov.out, trt = "leaching.data.Barley$Treatment")
Barley.HSD.data <- HSD.out$groups[c('Control', '50mM', '100mM', '200mM'),]
names(Barley.HSD.data) <- c('Above-ground oven-dried weight', 'groups')
Barley.HSD.data

#Also use Kruskal-Wallis test.
kruskal.test(leaching.data.Barley$ABOVE.Oven.g. ~ leaching.data.Barley$Treatment) #Significant!
#Post-hoc test for Kruskal-Wallis test.
library(dunn.test)
dunn.out <- dunn.test(x = leaching.data.Barley$ABOVE.Oven.g., g = leaching.data.Barley$Treatment)
library(multcomp)
#cld(dunn.out)
#install.packages('PMCMR')
library(PMCMR)
#leaching.data.Barley$Treatment <- factor(leaching.data.Barley$Treatment, levels = c('Control', '50mM', '100mM', '200mM'))
#dunn.test.control(x = leaching.data.Barley$ABOVE.Oven.g., g = leaching.data.Barley$Treatment, p.adj = 'none')

kruskal.out <- kruskal(y = leaching.data.Barley$ABOVE.Oven.g., trt = leaching.data.Barley$Treatment, p.adj = 'fdr')
kruskal.out$groups[c('Control', '50mM', '100mM', '200mM'),]








#SugarBeet
#Check assumptions of ANOVA:  homogenity of variance.  normally distributed data.
leaching.data.SugarBeet

library(ggplot2)

ggplot(data=leaching.data.SugarBeet, aes(leaching.data.SugarBeet$ABOVE.Oven.g.)) + 
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~Treatment) +
  xlab('Above-ground oven-dried weight (g)') +
  ggtitle('Leaching Sugar Beet -- histograms')

ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/leaching.SugarBeet.histogram.pdf', width = 5, height = 4)

library(car)
leveneTest(leaching.data.SugarBeet$ABOVE.Oven.g. ~ leaching.data.SugarBeet$Treatment)

#Variance homogeneous.  Try ANOVA
aov.out <- aov(leaching.data.SugarBeet$ABOVE.Oven.g. ~ leaching.data.SugarBeet$Treatment)
anova(aov.out) #Not significant!

#Do a post-hoc test to see which groups are different.
HSD.out <- HSD.test(y = aov.out, trt = "leaching.data.SugarBeet$Treatment")
SugarBeet.HSD.data <- HSD.out$groups[c('Control', '50mM', '100mM', '200mM'),]
names(SugarBeet.HSD.data) <- c('Above-ground oven-dried weight', 'groups')
SugarBeet.HSD.data

#Also use Kruskal-Wallis test.
kruskal.test(leaching.data.SugarBeet$ABOVE.Oven.g. ~ leaching.data.SugarBeet$Treatment) #Significant!
#Post-hoc test for Kruskal-Wallis test.
library(dunn.test)
dunn.out <- dunn.test(x = leaching.data.SugarBeet$ABOVE.Oven.g., g = leaching.data.SugarBeet$Treatment)
library(multcomp)
#cld(dunn.out)
#install.packages('PMCMR')
library(PMCMR)
#leaching.data.SugarBeet$Treatment <- factor(leaching.data.SugarBeet$Treatment, levels = c('Control', '50mM', '100mM', '200mM'))
#dunn.test.control(x = leaching.data.SugarBeet$ABOVE.Oven.g., g = leaching.data.SugarBeet$Treatment, p.adj = 'none')

kruskal.out <- kruskal(y = leaching.data.SugarBeet$ABOVE.Oven.g., trt = leaching.data.SugarBeet$Treatment, p.adj = 'fdr')
kruskal.out$groups[c('Control', '50mM', '100mM', '200mM'),]








#Alfalfa
leaching.data.Alfalfa

#Check assumptions of ANOVA:  homogenity of variance.  normally distributed data.

library(ggplot2)

ggplot(data=leaching.data.Alfalfa, aes(leaching.data.Alfalfa$ABOVE.Oven.g.)) + 
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~Treatment) +
  xlab('Above-ground oven-dried weight (g)') +
  ggtitle('Leaching Alfalfa -- histograms')

ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/leaching.Alfalfa.histogram.pdf', width = 5, height = 4)

library(car)
leveneTest(leaching.data.Alfalfa$ABOVE.Oven.g. ~ leaching.data.Alfalfa$Treatment)

#Variance homogeneous.  Try ANOVA
aov.out <- aov(leaching.data.Alfalfa$ABOVE.Oven.g. ~ leaching.data.Alfalfa$Treatment)
anova(aov.out) #Not significant!

#Do a post-hoc test to see which groups are different.
HSD.out <- HSD.test(y = aov.out, trt = "leaching.data.Alfalfa$Treatment")
Alfalfa.HSD.data <- HSD.out$groups[c('Control', '50mM', '100mM', '200mM'),]
names(Alfalfa.HSD.data) <- c('Above-ground oven-dried weight', 'groups')
Alfalfa.HSD.data

#Also use Kruskal-Wallis test.
kruskal.test(leaching.data.Alfalfa$ABOVE.Oven.g. ~ leaching.data.Alfalfa$Treatment) #Significant!
#Post-hoc test for Kruskal-Wallis test.
library(dunn.test)
dunn.out <- dunn.test(x = leaching.data.Alfalfa$ABOVE.Oven.g., g = leaching.data.Alfalfa$Treatment)
library(multcomp)
#cld(dunn.out)
#install.packages('PMCMR')
library(PMCMR)
#leaching.data.Alfalfa$Treatment <- factor(leaching.data.Alfalfa$Treatment, levels = c('Control', '50mM', '100mM', '200mM'))
#dunn.test.control(x = leaching.data.Alfalfa$ABOVE.Oven.g., g = leaching.data.Alfalfa$Treatment, p.adj = 'none')

kruskal.out <- kruskal(y = leaching.data.Alfalfa$ABOVE.Oven.g., trt = leaching.data.Alfalfa$Treatment, p.adj = 'fdr')
kruskal.out$groups[c('Control', '50mM', '100mM', '200mM'),]








#S.Europaea
leaching.data.S.Europaea

#Check assumptions of ANOVA:  homogenity of variance.  normally distributed data.
library(ggplot2)

ggplot(data=leaching.data.S.Europaea, aes(leaching.data.S.Europaea$ABOVE.Oven.g.)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~Treatment) +
  xlab('Above-ground oven-dried weight (g)') +
  ggtitle('Leaching S.Europaea -- histograms')

ggsave('~/Dropbox/Bo Hu/Noah/Leif/R images/leaching.S.Europaea.histogram.pdf', width = 5, height = 3)

library(car)
leveneTest(leaching.data.S.Europaea$ABOVE.Oven.g. ~ leaching.data.S.Europaea$Treatment)

#Variance homogeneous.  Try ANOVA
aov.out <- aov(leaching.data.S.Europaea$ABOVE.Oven.g. ~ leaching.data.S.Europaea$Treatment)
anova(aov.out) #Not Significant!


#Also use Kruskal-Wallis test.
kruskal.test(leaching.data.S.Europaea$ABOVE.Oven.g. ~ leaching.data.S.Europaea$Treatment) #Significant!
