load("~/SentimentIt/data/fit_stanDataExample.rda")
fit <- fit_stan(ExampleData)
check_workers(fit, data, plot=TRUE)
