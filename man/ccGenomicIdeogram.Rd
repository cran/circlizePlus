% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track.R
\name{ccGenomicIdeogram}
\alias{ccGenomicIdeogram}
\title{Define an ideograms track for genomic graph}
\usage{
ccGenomicIdeogram(
  cytoband = system.file(package = "circlize", "extdata", "cytoBand.txt"),
  species = NULL,
  track.height = mm_h(2),
  track.margin = circos.par("track.margin")
)
}
\arguments{
\item{cytoband}{A data frame or a file path, pass to \code{\link[circlize]{read.cytoband}}.
}

\item{species}{Abbreviations of the genome, pass to \code{\link[circlize]{read.cytoband}}.
}

\item{track.height}{Height of the ideogram track.
}

\item{track.margin}{Margins for the track.
}
}
\value{
Object \linkS4class{ccGenomicTrack}
}
\description{
Object \linkS4class{ccGenomicTrack} will call the function \link[circlize:circos.genomicIdeogram]{circlize::circos.genomicIdeogram} while drawing.
}
\examples{
\donttest{
library(circlizePlus)
cc = ccPlot(initMode = "initializeWithIdeogram",chromosome.index = "chr1", plotType = NULL)
human_cytoband = read.cytoband(species = "hg19")$df
t2=ccGenomicIdeogram(human_cytoband)
cc+t2
circos.clear()
}
}
