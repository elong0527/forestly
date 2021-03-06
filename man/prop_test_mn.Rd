% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prop_test_mn.R
\name{prop_test_mn}
\alias{prop_test_mn}
\title{Test of Equal Proportion Using Miettinen and Nurminen Method}
\usage{
prop_test_mn(x0, n0, x1, n1, delta = 0, sides = "two.sided", alpha = 0.05)
}
\arguments{
\item{x0}{number of successes for the first group.}

\item{n0}{number of trials for the first group.}

\item{x1}{number of successes for the second group.}

\item{n1}{number of trials for the second group.}

\item{delta}{assumed proportion difference}

\item{sides}{specify the hypothesis directions: two.sided, righ(less than), left(greater than)}

\item{alpha}{pre-difined significant level}
}
\description{
\enumerate{
\item Assign numbers and define parameters
\item Perform algebra for calculating maximum likelihood estimates of proportion on treatment and control groups.
\item Calculate sample variance based on step 2 and constract test-statistics
\item For non-inferiority or one-sided equivalence hypothesis, the p-value is computed based on the test-statistic (Zdiff) using the standard normal distribution.
\item For superiority or two-sided test, the p-value is computed based on the test statistic (Zdiff^2) using the Chi-square distribuiton with df = 1.
}
}
\examples{

x0 <- c(1,2); n0 <- c(100, 100);
x1 <- c(4,5); n1 <- c(100, 100);
prop_test_mn(x0, n0, x1, n1)

}
