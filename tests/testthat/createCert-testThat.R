# Creating Unit Tests - createCert
library("testthat")

# Checks for proper inputs of certifications
context("Correct Certifications")
# Inputs a radnom worker ID as the constant.
test_that("Proper Certifications", {
  w1 <- function(x){
    return("abcd")
  }
  # Ensures no blank certification
  expect_error(createCert(cert = "", workers = w1),
               "You must input a non-blank certification to be created for the workers")
  # Ensures a numeric input for the certification
  expect_error(createCert(cert = "ab", workers = w1),
               "You must input a numeric certification for the workers")
  # Ensures a positive, whole number certification
  expect_error(createCert(cert = -3.1, workers = w1),
               "You must input a whole, positive number to represent the ceritification input.")
  # Ensures one certification must be added at a time.
  expect_error(createCert(cert = c(2,3,4), workers = w1),
               "You can only grant one certification at a time.")
})

test_that("Proper Workers",{
  c1 <- function(x){
    return("22")
  }
  # Ensures a non-blank worker ID.
  expect_error(createCert(cert = c1, workers = ""),
               "You must input a non-blank worker ID.")
})

