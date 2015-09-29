setwd('C:/Users/Felix/Desktop/Free Food Listserv') # <------ type directory containing freefood.csv file here
install.packages(c('qgraph', 'rpart', 'tm'))

data = read.csv('freefood.csv', stringsAsFactors = F)
data$date = as.Date(data$date)
head(data)

school.days = seq(as.Date('2014-09-10'), as.Date('2014-10-24'), by = 1)
school.days = c(school.days, seq(as.Date('2014-11-03'), as.Date('2014-11-25'), by = 1))
school.days = c(school.days, seq(as.Date('2014-12-01'), as.Date('2014-12-12'), by = 1))
school.days = c(school.days, seq(as.Date('2015-02-02'), as.Date('2015-03-14'), by = 1))
school.days = c(school.days, seq(as.Date('2015-03-23'), as.Date('2015-05-03'), by = 1))

data = data[data$date %in% school.days,]

nrow(data) / length(school.days)

# Create a table of 0s
count = rep(0, length(school.days))
names(count) = school.days

# Fill in some 0s with the number of emails sent out that day
count[names(table(data$date))] = table(data$date)

# Verify the mean is the same
mean(count)

hist(count)

feast.day = names(which.max(count))
data[data$date == feast.day,]

# split data frame to list of data frames by day of week
day.of.week = split(data, weekdays(data$date))

# order list Sunday, Monday, ... Saturday
dow = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
day.of.week = day.of.week[dow]

# create table for number of emails sent and total number of days
emails = sapply(day.of.week, nrow)
days   = table(weekdays(school.days))
days   = days[dow]

barplot(emails / days, main = 'Avg # emails each day of week')

# create a function to compute standard error for a
# set of emails sent on a particular day
compute.se = function(df)
{
  dow = unique(weekdays(df$date))
  stopifnot(length(dow) == 1)
  
  days = school.days[weekdays(school.days) == dow]
  n = length(days)
  
  count = rep(0, length(days))
  names(count) = days
  count[names(table(df$date))] = table(df$date)
  
  return( sd(count) / sqrt(n) )  
}

se = sapply(day.of.week, compute.se)

# barplot( ) returns horizontal position of the days
x  = barplot(emails / days, main = 'Avg # emails each day of week', ylim = c(0, 5))
arrows(x, emails / days + se, x, emails / days - se, angle = 90, code = 3, length = 0.1)

# Given data frame of emails, return vector of words used sorted most freq first
subject.line.words = function(data)
{
  words = unlist(lapply(data$subj, function(subj) strsplit(subj, ' ')))  # split subject line into individual words
  words = words[words != '[FreeFood]']
  words = tolower(words)  # make all words lowercase
  words = sapply(words, function(word) gsub("[[:punct:]]", "", word))  # delete any punctuation marks
  words.freq = table(words)
  return(sort(words.freq, decreasing = T))
}

subject.line.words(data[data$hour < 15,])[1:30]

library(tm)
subject.lines = gsub('[[:punct:]]', ' ', data$subj)
subject.lines = gsub('FreeFood', '', subject.lines)
corpus = VCorpus(VectorSource(subject.lines))
tdm = TermDocumentMatrix(corpus)
tdm = as.data.frame(t(as.matrix(tdm)))

library(rpart)
tdm$y = data$hour + data$min / 60
tree = rpart(y ~ ., data = tdm)
par(xpd = NA)
plot(tree)
text(tree)

m = as.matrix(tdm[,-ncol(tdm)]) > 0
colsums = apply(m, 2, sum)
m = m[, colsums >= 6]

X = t(m) %*% m
Y = t(!m) %*% m
A = X / (X + Y + t(Y))

library(qgraph)
qgraph(A, minimum = 0.15, border.color = 'Gray',
       labels = colnames(A), label.scale = F, label.cex = 0.8)
