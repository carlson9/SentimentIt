load("~/SentimentIt/data/fit_stanDataExample.rda")
fit <- fitStan(ExampleData)
checkWorkers(stan_fit=fit, data=data, cut_point=1, cut_proportion=0.9, n.questions=50, plot_hist=FALSE, file_path=NULL)