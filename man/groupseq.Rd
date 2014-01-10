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
groupseq(mode = chosenMode)
}

\arguments{
  \item{mode}{ Default is mode="g" (recommended) to use the gui \cr
               but user may choose mode="c" to use the R console, in case the gui does not work.}
}

\details{
GroupSeq is appropriate for any trial which is based on normally distributed test statistics with independent 
increments, for survival studies, and certain longitudinal designs.
Among other things it computes critical boundaries for various spending functions and for prespecified
power and drift. Confidence intervals are also obtained.

The GUI is build within the Tcl/Tk interface of R. By only using the standard Tcl/Tk package, 
no additional Tcl/Tk packages have to be installed for the GUI to work. This ensures maximal compatibility and
moreover the GUI should be available for every platform on which R runs. Customization within the GUI is left to the user 
who may create separate windows for each task which allows any arrangement and positioning in working space.
Thus, the user may perform multiple tasks e.g. computing and comparing several designs at the same time.
}

\references{ \url{http://www.biostat.wisc.edu/landemets/} }

\author{ Roman Pahl }

\examples{
## call GUI mode (default)
# groupseq() or groupseq(mode="g")

## call console mode
# groupseq(mode="c")
}

\keyword{methods}