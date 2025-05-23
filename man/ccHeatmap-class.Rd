% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmap.R
\docType{class}
\name{ccHeatmap-class}
\alias{ccHeatmap-class}
\title{S4 class ccHeatmap}
\description{
ccHeatmap is a special class. It can be used not only as a single track but also as the result of adding a heatmap track to a ccPlot
}
\section{Slots}{

\describe{
\item{\code{func}}{character. Normally it is "circos.heatmap".}

\item{\code{params}}{list. A \strong{named} list that stores the parameters of the function \link[circlize:circos.heatmap]{circlize::circos.heatmap} called by the backend.}

\item{\code{trackGeoms}}{list. A list where \linkS4class{ccTrackGeom} are stored.}

\item{\code{cells}}{list. A list where \linkS4class{ccCell} are stored.}

\item{\code{tracks}}{list. A list where \linkS4class{ccTrack} or \linkS4class{ccGenomicTrack} or \linkS4class{ccHeatmap} are stored.}

\item{\code{links}}{list. A list where \linkS4class{ccLink} or \linkS4class{ccGenomicLink} or \linkS4class{ccHeatmapLink} are stored.}

\item{\code{pars}}{list. A list where \linkS4class{ccPar} are stored.}

\item{\code{clear}}{logical. Whether to call \link[circlize:circos.clear]{circlize::circos.clear} before drawing.}
}}

