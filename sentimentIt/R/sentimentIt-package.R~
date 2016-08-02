#' sentimentIt
#'
#' The sentimentIt package interacts with the SentimentIt API to analyze text and capture latent traits of interest.
#' @name sentimentIt
#' @docType package
#' @author  David Carlson \email{carlson.david@@wustl.edu} and Jacob M. Montgomery \email{jacob.montgomery@@wustl.edu}
#' @details
#' The SentimentIt system is designed to estimate a researcher-specified latent trait of interest for text corpora. The system is introduced in Montgomery and Carlson ''Human computation scaling for measuring meaningful latent traits in political texts.'' The system crowd-sources pairwise comparisons of hte documents, currently through Amazon Mechanical Turk. This is a cognitively simple task and much easier for workers to achieve reliably than thermometer-type coding or similar approaches. The workers are simply asked which of the two documents is further along some dimension of interest, e.g. positivity. Once a sufficient number of comparisons are done per document (when randomly selecting comparisons we find 20 is a reasonable number), the traits are estimated using a random utility model via Hamiltonian MCMC in Stan. This process generates both point estimates and full posterior distributions. This is a reliable and efficient way to estimate the underlying sentiment in text, a task often too difficult for machines and too time-consuming, costly, and unreliable for expert coders.
#'
#' This package is designed to allow nearly full implementation of the system. Some start-up is necessary outside of the R environment. To begin using SentimentIt, navigate to \url{https://github.com/carlson9/SentimentIt-Public}. Here you will find detailed information on setting up a SentimentIt account, setting up an MTurk account, linking the two, setting up certifications and training for workers, and creating HIT settings. Once these steps have been completed, all that is needed is the HIT Setting ID number you wish to use for your task. You can then proceed to use the package. The documentation also explains the purpose of these settings and guidance for best practices.
#'
#' First, the base-level functionality of the package is explained to give an intuition behind the process. We then show how the entire process can be done using the wrapper function \code{\link{sentimentIt}}. All argument names are the same in the wrapper function as they are in base-level functions. Data on Rotten Tomato movie reviews comes with the package (\code{\link{reviews}}) and we will use this data to demonstrate how to uncover the positivity of a movie review which can then be benchmarked to the stars the reviewer provided.
#'
#' We begin by reading in the data using the \code{\link{readText}} function. If we want to use the movie review data in the package, we could run the following code:
#'
#' \code{data(reviews)}
#' \code{readText(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    read_documents_from = reviews,
#'    write_documents_to = "ReviewsWithIds.csv",
#'    index = 'Review')}
NULL
