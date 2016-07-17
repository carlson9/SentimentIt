load("~/SentimentIt/data/fit_stan_hierDataExample.rda")
fit_hier <- fitStanHier(data=ExampleData_hier1, hierarchy_data=ExampleData_hier2, hierarchy_var="countries")
extractCoef(fit_hier, ExampleData_hier2, "ids", "countries")