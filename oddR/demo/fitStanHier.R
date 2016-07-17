load("~/SentimentIt/data/fit_stan_hierDataExample.rda")
fitStanHier(data=ExampleData_hier1, hierarchy_data=ExampleData_hier2, hierarchy_var="countries", chains=3, iter=2500, seed=1234, n.cores=3)
