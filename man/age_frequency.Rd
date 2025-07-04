% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_frequency.R
\name{age_frequency}
\alias{age_frequency}
\title{Age frequency function. Plots distribution of ages across survey years, and can differentiate by sex.}
\usage{
age_frequency(data, sex)
}
\arguments{
\item{data}{biological data from pull_bio(), containing Age_years}

\item{sex}{choose "y" or "n" for if you want to differentiate by sex}
}
\value{
a ggplot object
}
\description{
Age frequency function. Plots distribution of ages across survey years, and can differentiate by sex.
}
\examples{
\dontrun{
age_frequency(bio_data, sex = "n")
}
}
