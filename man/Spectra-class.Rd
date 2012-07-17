\name{Spectra-class}
\Rdversion{1.1}
\docType{class}
\alias{Spectra-class}
\alias{show,Spectra-method}

\title{Class \code{"Spectra"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Spectra", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{DF}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{Wavelengths}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{Units}:}{Object of class \code{"character"} ~~ }
    \item{\code{SelectedIdx}:}{Object of class \code{"integer"} ~~ }
    \item{\code{InvalidIdx}:}{Object of class \code{"integer"} ~~ }
    \item{\code{ShortName}:}{Object of class \code{"character"} ~~ }
    \item{\code{LongName}:}{Object of class \code{"character"} ~~ }
    \item{\code{Ancillary}:}{Object of class \code{"data.frame"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Spectra")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("Spectra")
}
\keyword{classes}
