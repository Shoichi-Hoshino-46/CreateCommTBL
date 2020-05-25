\name{CreateCommTBL}
\alias{CreateCommTBL}
\title{Create commutation table}
\usage{
cal_comm(qx,i)
}
\arguments{
\item{qx}{vector of mortality}

\item{i}{scalar of interest rate}
}
\description{
Returns the commutation table for life insurance.
}
\value{
Commutation table
}
\examples{
qx <- c(0.00081,0.00056,0.00038)
i <- 0.05
}
