\name{groupseq}

\alias{groupseq}

\title{ Starts the program. }
\description{
This function is invoked when the package is loaded into R workspace. It starts the main menu
using a graphical user interface (GUI). If, for any reason, the GUI does not work,
user can choose an interactive mode by typing groupseq(mode="c") to work within the R console.
However, the console mode has not been further developed since GroupSeq version 1.0.

The program itself refers to an earlier Fortran Implementation by Reboussin et al.
and was completely recoded in R. Following the maxim of modular programming,
it was decomposed in various functions. Furthermore, some algorithms could be improved
and the whole application was embedded in a GUI affording much more convenient and faster working.
Last but not least, computation results can be saved into common tables of *.html file format
to eventually process them further on.
}




\usage{
groupseq(mode = "g")
}

\arguments{
  \item{mode}{ Default is mode="g" (recommended) to use the gui \cr
               but user may choose mode="c" to use the R console, in case the gui does not work.}
}

\details{
GroupSeq is appropriate for any trial which is based on normally distributed
test statistics with independent increments, for survival studies, and certain
longitudinal designs. Among other things, it computes critical boundaries for
various spending functions and for prespecified power and drift. Confidence
intervals are also obtained. Since all computations are done via the alpha
spending approach by Lan-DeMets, interim analyses not necessarily need to be
equally spaced, and their number need not to be specified in advance,
respectively.

The GUI is build within the Tcl/Tk interface of R and therefore basically should
work smoothly at every platform R runs on. Customization within the GUI is left
to the user, who may create separate windows for each task so that multiple tasks
(e.g., computing and comparing several designs) can be performed simultanously.
}

\references{ \url{http://www.biostat.wisc.edu/Software/landemets/index.html} }

\author{ Roman Pahl }

\examples{
## call GUI mode (default)
# groupseq() or groupseq(mode="g")

## call console mode
# groupseq(mode="c")
}

\keyword{methods}
