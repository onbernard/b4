% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FMA_TS_wrappers.R
\name{b4_ThompsonSampling}
\alias{b4_ThompsonSampling}
\title{ThompsonSampling}
\usage{
b4_ThompsonSampling(visitor_reward, alpha = 1, beta = 1, bandit = NULL)
}
\arguments{
\item{visitor_reward}{Dataframe of integer or numeric values}

\item{alpha}{Numeric value (optional)}

\item{beta}{Numeric value (optional)}
}
\description{
A thompson sampling (TS) bandit strategy implemented by sampling, in each round, averages from a posterior
distribution  \code{\link{ConditionForThompsonSampling}}, and choosing the action that maximizes the expected reward given the
sampled average. Conceptually, this means that the player instantiates their beliefs
randomly in each round, and then acts optimally according to them.
Generate a matrix to save the results (S).
\itemize{ At each iteration
\item Sample an averages from a posterior in S for each arm (beta distribution with alpha and beta parameters)
\item Choose the arm with the highest average
\item Receives a reward in visitor_reward for the arm and associated iteration
\item Updates the results matrix S.
}
}
\examples{
## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitor_reward <- as.data.frame( cbind(K1,K2) )
b4_ThompsonSampling(visitor_reward)
}
