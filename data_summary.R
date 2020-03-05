# Author: Josh Arnold
# Data Summary

# InstEval Data summary
library(lme4)
data <- InstEval
summary(data)

# Song Dataset Summary
songs <- read.csv(file = 'songsDataset.csv')
summary(songs)

?InstEval

# Histogram
library(ggplot2)
ggplot(InstEval, aes(x=y)) + geom_histogram(binwidth=0.5) + theme_light()
ggplot(songs, aes(x=X.rating.)) + geom_histogram(binwidth=0.5) + theme_light()

head(songs)
head(InstEval)