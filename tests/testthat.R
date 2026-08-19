library(testthat)
library(grout)

# Route any plotting during tests to a null device so running the suite
# doesn't leave a stray Rplots.pdf behind.
grDevices::pdf(NULL)

test_check("grout")
