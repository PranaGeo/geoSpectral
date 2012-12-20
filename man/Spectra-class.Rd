\name{Spectra-class}
\Rdversion{1.1}
\docType{class}
\alias{Spectra-class}
\alias{[,Spectra-method}
\alias{[[<-,Spectra,ANY,ANY-method}
\alias{[[,Spectra,ANY,ANY-method}
\alias{$<-,Spectra-method}
\alias{$,Spectra-method}
\alias{bioo.add.column,Spectra-method}
\alias{bioo.setheader<-,Spectra-method}
\alias{bioo.updateheader<-,Spectra-method}
\alias{dim,Spectra-method}
\alias{head,Spectra-method}
\alias{names<-,Spectra-method}
\alias{names,Spectra-method}
\alias{ncol,Spectra-method}
\alias{nrow,Spectra-method}
\alias{rep,Spectra-method}
\alias{spc.add.channel,Spectra-method}
\alias{spc.cname.construct,Spectra-method}
\alias{spc.getancillary,Spectra-method}
\alias{spc.getwavelengths,Spectra-method}
\alias{spc.interp.spectral,Spectra-method}
\alias{spc.lines,Spectra-method}
\alias{spc.plot,Spectra-method}
\alias{spc.rbind,Spectra-method}
\alias{spc.select,Spectra-method}
\alias{spc.setancillary<-,Spectra-method}
\alias{spc.setwavelengths<-,Spectra-method}
\alias{subset,Spectra-method}

\title{Class \code{"Spectra"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Spectra", DF, ShortName, LongName, Wavelengths, Units, Ancillary, header, WavelengthUnit, ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{ShortName}:}{Object of class \code{"character"} ~~ }
    \item{\code{Wavelengths}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{WavelengthUnit}:}{Object of class \code{"character"} ~~ }
    \item{\code{Ancillary}:}{Object of class \code{"Bioo"} ~~ }
    \item{\code{DF}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{header}:}{Object of class \code{"BiooHeader"} ~~ }
    \item{\code{Units}:}{Object of class \code{"character"} ~~ }
    \item{\code{LongName}:}{Object of class \code{"character"} ~~ }
    \item{\code{SelectedIdx}:}{Object of class \code{"logical"} ~~ }
    \item{\code{InvalidIdx}:}{Object of class \code{"logical"} ~~ }
    \item{\code{ProcessLog}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Bioo}"}, directly, with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Spectra")}: ... }
    \item{[[<-}{\code{signature(x = "Spectra", i = "ANY", j = "ANY")}: ... }
    \item{[[}{\code{signature(x = "Spectra", i = "ANY", j = "ANY")}: ... }
    \item{$<-}{\code{signature(x = "Spectra")}: ... }
    \item{$}{\code{signature(x = "Spectra")}: ... }
    \item{bioo.add.column}{\code{signature(object = "Spectra")}: ... }
    \item{bioo.setheader<-}{\code{signature(object = "Spectra")}: ... }
    \item{bioo.updateheader<-}{\code{signature(object = "Spectra")}: ... }
    \item{dim}{\code{signature(x = "Spectra")}: ... }
    \item{head}{\code{signature(x = "Spectra")}: ... }
    \item{names<-}{\code{signature(x = "Spectra")}: ... }
    \item{names}{\code{signature(x = "Spectra")}: ... }
    \item{ncol}{\code{signature(x = "Spectra")}: ... }
    \item{nrow}{\code{signature(x = "Spectra")}: ... }
    \item{rep}{\code{signature(x = "Spectra")}: ... }
    \item{spc.add.channel}{\code{signature(object = "Spectra")}: ... }
    \item{spc.cname.construct}{\code{signature(object = "Spectra")}: ... }
    \item{spc.getancillary}{\code{signature(object = "Spectra")}: ... }
    \item{spc.getwavelengths}{\code{signature(object = "Spectra")}: ... }
    \item{spc.interp.spectral}{\code{signature(source1 = "Spectra")}: ... }
    \item{spc.lines}{\code{signature(x = "Spectra")}: ... }
    \item{spc.plot}{\code{signature(x = "Spectra")}: ... }
    \item{spc.rbind}{\code{signature(... = "Spectra")}: ... }
    \item{spc.select}{\code{signature(object = "Spectra")}: ... }
    \item{spc.setancillary<-}{\code{signature(object = "Spectra")}: ... }
    \item{spc.setwavelengths<-}{\code{signature(object = "Spectra")}: ... }
    \item{subset}{\code{signature(x = "Spectra")}: ... }
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
