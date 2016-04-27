load("~/SentimentIt/data/fit_stanDataExample.rda")
stanWrapper(ExampleData, hier_data=NULL, hierarchy_var=NULL, returnFit=FALSE, cut_point=1, cut_percent=0.9, plot_hist=FALSE, file_path=NULL, chains=3, iter=2500, seed=1234, n.cores=3)
