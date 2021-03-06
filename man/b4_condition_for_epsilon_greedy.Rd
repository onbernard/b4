% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FMA_EPSG_policy.R
\name{b4_condition_for_epsilon_greedy}
\alias{b4_condition_for_epsilon_greedy}
\title{Condition for Epsilon Greedy}
\usage{
b4_condition_for_epsilon_greedy(S, epsilon = 0.25, K = ncol(S))
}
\arguments{
\item{S}{FMA regression parameter}

\item{epsilon}{Epsilon Greedy exploration parameter}

\item{K}{number of arms}
}
\description{
Return the next arm to be pulled
}
