load("~/SentimentIt/data/fit_stanDataExample.rda")
fit <- fitStan(ExampleData)
checkWorkers(fit, data, plot=TRUE)
