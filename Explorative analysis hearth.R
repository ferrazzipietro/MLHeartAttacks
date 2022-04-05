library(tidyverse)
library(gridExtra)
dat <- read.csv(file.choose(), sep=";") #heart....
dat <- dat %>% as_tibble()
str(dat)

# trasform in factors categorical variables
dat <- dat%>% mutate(Event=as.factor(dat$Event),
                     Gender=as.factor(Gender),
                     Smoking=as.factor(Smoking),
                     Diabetes=as.factor(Diabetes),
                     BP=as.factor(BP),
                     Anaemia=as.factor(Anaemia))

###########################################
# explorative analysis
###########################################

####
# 
###
View(dat)
g <- ggplot(dat, aes(y=TIME))

pairs(dat %>% select(Age, Ejection.Fraction, Sodium, Creatinine))

g + geom_boxplot(aes(x=as.factor(Event)))# What is event?
g + geom_boxplot(aes(x=as_factor(Gender)))
g + geom_boxplot(aes(x=as_factor(Smoking)))
g + geom_boxplot(aes(x=as_factor(Diabetes)))
g + geom_boxplot(aes(x=as_factor(BP)))
g + geom_boxplot(aes(x=as_factor(Anaemia)))
# it doesn't look  interesting to distinguish between different levels of 
# the variables

ggplot(dat) + geom_bar(aes(Gender), stat="count",
                             width=0.2, color=1, fill="white") 
ggplot(dat) + geom_bar(aes(Smoking), stat="count",
                       width=0.2, color=1, fill="white") 
ggplot(dat) + geom_bar(aes(Diabetes), stat="count",
                       width=0.2, color=1, fill="white") 
ggplot(dat) + geom_bar(aes(BP), stat="count",
                       width=0.2, color=1, fill="white") 
ggplot(dat) + geom_bar(aes(Anaemia), stat="count",
                       width=0.2, color=1, fill="white") 
# everything looks balanced

# looking for patterns in the data
col <- scale_color_gradient(high="red", low = "white", 
                            n.breaks=4,
                            breaks = waiver())


ggplot(dat) + geom_point(aes(x=Sodium, y=Age, col=TIME),
                         lwd=3) + col
ggplot(dat) + geom_point(aes(x=Sodium, y=Pletelets, col=TIME),
                         lwd=3) + col
ggplot(dat) + geom_point(aes(x=Sodium, y=CPK, col=TIME),
                         lwd=3) + col
ggplot(dat) + geom_point(aes(x=CPK, y=Age, col=TIME),
                         lwd=3) + col
ggplot(dat) + geom_point(aes(x=Pletelets, y=Age, col=TIME),
                         lwd=3) + col
ggplot(dat) + geom_point(aes(x=Pletelets, y=CPK, col=TIME),
                         lwd=3) + col



