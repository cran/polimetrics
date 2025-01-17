% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ideo.dist.R
\name{ideo.dist}
\alias{ideo.dist}
\title{Calculating Ideological Distance}
\usage{
ideo.dist(df, id, ideology, time, adoption)
}
\arguments{
\item{df}{data frame to read in. This should be an adapted version of the \code{Ideology} data set provided in the package. The adapted version should include an outcome variable measuring the policy adoption of choice.}

\item{id}{the grouping variable, usually states}

\item{ideology}{the state's ideology score variable (either \emph{state} or \emph{citizen} ideology) in a given year. These data come from Richard C. Fording (\url{https://rcfording.com/state-ideology-data/}) as used in Berry et al. (1998), and are measured, for each state, from 1960 to 2018.}

\item{time}{the time variable, at which the ideology score is measured. These data come from Richard C. Fording (\url{https://rcfording.com/state-ideology-data/}) as used in Berry et al. (1998), and are measured, for each state, from 1960 to 2018.}

\item{adoption}{binary, user-defined measure of policy adoption in a state in a given year. \code{0} equates to \emph{policy not adopted in the year, for the state}, \code{1} equates to \emph{policy is adopted in the year, for the state} -- a value of \code{1} should only exist for a state in the year it was adopted (e.g. not every year thereafter). The example below relies on ERA ratification data from Soule and King (2006), but the user should include the measure of adoption of their choice.}
}
\value{
This function updates the data frame with a new variable capturing the ideological distance score.
}
\description{
Calculating Ideological Distance
}
\examples{
data <- Ideology_ERA

ideo.dist(data, state, s_ideo, year, era_ratified)
}
\references{
Grossback, Lawrence J., Nicholson-Crotty, Sean, and 
Peterson, David A.M. 
(2004) 'Ideology and Learning in Policy Diffusion.' 
\emph{American Politics Research} 32:521-545.
\doi{10.1177/1532673X04263801}.\cr
Cruz-Aceves, Victor D., and Mallinson, Daniel J.
(2019) 'Clarifying the Measurement of Relative Ideology 
in Policy Diffusion Research.' 
\emph{State and Local Government Review} 51:179-186.
\doi{10.1177/0160323X20902818}.\cr
Berry, William D., Ringquist, Evan J., Fording, Richard C.,
and Hanson, Russell L.
(1998) 'Measuring Citizen and Government Ideology 
in the American States, 1960-93.' 
\emph{American Journal of Political Science} 42:327-348.
\doi{10.2307/2991759}.\cr
Soule, Sarah A., and King, Brayden G.
(2006) 'The Stages of the Policy Process 
and the Equal Rights Amendment, 1972-1982.' 
\emph{American Journal of Sociology} 111:1871-1909.
\doi{10.1086/499908}.\cr

This function calculates ideological distance scores based on the calculation created by Grossback et al. (2004) and clarified by Cruz-Aceves and Mallinson (2019). This calculation is based on state ideology data (by year) provided by Richard C. Fording (\url{https://rcfording.com/state-ideology-data/}) and used in Berry et al. (1998). This function can be applied to any unit of analysis and time level for any type of policy adoption.
}
