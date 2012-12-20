\name{Bioo-class}
\Rdversion{1.1}
\docType{class}
\alias{Bioo-class}
\alias{[,Bioo-method}
\alias{[[<-,Bioo,ANY,ANY-method}
\alias{[[,Bioo,ANY,ANY-method}
\alias{$<-,Bioo-method}
\alias{$,Bioo-method}
\alias{bioo.add.column,Bioo-method}
\alias{bioo.data2header,Bioo-method}
\alias{bioo.getheader,Bioo-method}
\alias{bioo.getinvalid.idx,Bioo-method}
\alias{bioo.getselected.idx,Bioo-method}
\alias{bioo.interp.time,Bioo-method}
\alias{bioo.invalid.detect,Bioo-method}
\alias{bioo.setheader<-,Bioo-method}
\alias{bioo.setinvalid.idx<-,Bioo-method}
\alias{bioo.setselected.idx<-,Bioo-method}
\alias{bioo.updateheader<-,Bioo-method}
\alias{cbind,Bioo-method}
\alias{dim,Bioo-method}
\alias{head,Bioo-method}
\alias{names,Bioo-method}
\alias{ncol,Bioo-method}
\alias{nrow,Bioo-method}
\alias{plot.time,Bioo-method}
\alias{rep,Bioo-method}
\alias{sort,Bioo-method}
\alias{spc.plot.depth,Bioo-method}
\alias{spc.rbind,Bioo-method}
\alias{subset,Bioo-method}

\title{Class \code{"Bioo"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Bioo", DF, Units, LongName, header, ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{DF}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{header}:}{Object of class \code{"BiooHeader"} ~~ }
    \item{\code{Units}:}{Object of class \code{"character"} ~~ }
    \item{\code{LongName}:}{Object of class \code{"character"} ~~ }
    \item{\code{SelectedIdx}:}{Object of class \code{"logical"} ~~ }
    \item{\code{InvalidIdx}:}{Object of class \code{"logical"} ~~ }
    \item{\code{ProcessLog}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Bioo")}: ... }
    \item{[[<-}{\code{signature(x = "Bioo", i = "ANY", j = "ANY")}: ... }
    \item{[[}{\code{signature(x = "Bioo", i = "ANY", j = "ANY")}: ... }
    \item{$<-}{\code{signature(x = "Bioo")}: ... }
    \item{$}{\code{signature(x = "Bioo")}: ... }
    \item{bioo.add.column}{\code{signature(object = "Bioo")}: ... }
    \item{bioo.data2header}{\code{signature(object = "Bioo")}: ... }
    \item{bioo.getheader}{\code{signature(object = "Bioo")}: ... }
    \item{bioo.getinvalid.idx}{\code{signature(object = "Bioo")}: ... }
    \item{bioo.getselected.idx}{\code{signature(object = "Bioo")}: ... }
    \item{bioo.interp.time}{\code{signature(source1 = "Bioo")}: ... }
    \item{bioo.invalid.detect}{\code{signature(source1 = "Bioo")}: ... }
    \item{bioo.setheader<-}{\code{signature(object = "Bioo")}: ... }
    \item{bioo.setinvalid.idx<-}{\code{signature(object = "Bioo")}: ... }
    \item{bioo.setselected.idx<-}{\code{signature(object = "Bioo")}: ... }
    \item{bioo.updateheader<-}{\code{signature(object = "Bioo")}: ... }
    \item{cbind}{\code{signature(deparse.level = "Bioo")}: ... }
    \item{dim}{\code{signature(x = "Bioo")}: ... }
    \item{head}{\code{signature(x = "Bioo")}: ... }
    \item{names}{\code{signature(x = "Bioo")}: ... }
    \item{ncol}{\code{signature(x = "Bioo")}: ... }
    \item{nrow}{\code{signature(x = "Bioo")}: ... }
    \item{plot.time}{\code{signature(object = "Bioo")}: ... }
    \item{rep}{\code{signature(x = "Bioo")}: ... }
    \item{sort}{\code{signature(x = "Bioo")}: ... }
    \item{spc.plot.depth}{\code{signature(object = "Bioo")}: ... }
    \item{spc.rbind}{\code{signature(... = "Bioo")}: ... }
    \item{subset}{\code{signature(x = "Bioo")}: ... }
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
showClass("Bioo")
}
\keyword{classes}
