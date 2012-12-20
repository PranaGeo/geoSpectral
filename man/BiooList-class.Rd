\name{BiooList-class}
\Rdversion{1.1}
\docType{class}
\alias{BiooList-class}
\alias{$,BiooList-method}
\alias{bioo.data2header,BiooList-method}
\alias{bioo.getheader,BiooList-method}
\alias{bioo.invalid.detect,BiooList-method}
\alias{bioo.setheader<-,BiooList-method}
\alias{names,BiooList-method}
\alias{sort,BiooList-method}
\alias{spc.lapply,BiooList-method}
\alias{spc.plot.depth.overlay,BiooList-method}
\alias{spc.plot.grid,BiooList-method}
\alias{spc.plot.overlay,BiooList-method}
\alias{subset,BiooList-method}

\title{Class \code{"BiooList"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BiooList", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
    \item{\code{by}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{$}{\code{signature(x = "BiooList")}: ... }
    \item{bioo.data2header}{\code{signature(object = "BiooList")}: ... }
    \item{bioo.getheader}{\code{signature(object = "BiooList")}: ... }
    \item{bioo.invalid.detect}{\code{signature(source1 = "BiooList")}: ... }
    \item{bioo.setheader<-}{\code{signature(object = "BiooList")}: ... }
    \item{names}{\code{signature(x = "BiooList")}: ... }
    \item{sort}{\code{signature(x = "BiooList")}: ... }
    \item{spc.lapply}{\code{signature(X = "BiooList")}: ... }
    \item{spc.plot.depth.overlay}{\code{signature(object = "BiooList")}: ... }
    \item{spc.plot.grid}{\code{signature(x = "BiooList")}: ... }
    \item{spc.plot.overlay}{\code{signature(object = "BiooList")}: ... }
    \item{subset}{\code{signature(x = "BiooList")}: ... }
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
showClass("BiooList")
}
\keyword{classes}
