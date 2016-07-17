load("~/SentimentIt/data/fit_stanDataExample.rda")
fitStan(data=ExampleData, chains=3, iter=2500, seed=1234, n.cores=3)
