% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_grab.R
\name{get_GL}
\alias{get_GL}
\title{Get Geomorphic Road Analysis and Inventory Package Lite (GRAIP-Lite) Data}
\usage{
get_GL(
  gdb,
  layers = "WCATT_HUCs_GL_Data_USC_Units_R01",
  version = "v3",
  quiet = FALSE,
  simplify = TRUE,
  ...
)
}
\arguments{
\item{gdb}{A \code{character} of the GDB, e.g. \code{'R1'}.}

\item{layers}{A \code{character} of the layer(s) within the GDB, e.g. \code{'HUCs_GL_Runs_R01'} (default).}

\item{version}{A \code{character} indicating the data version, e.g. \code{'v3'}.}

\item{quiet}{A \code{logical}; suppress info on name, driver, size and spatial reference, or signaling no or multiple layers.}

\item{simplify}{A \code{logical}; whether to return a simplified list (\code{data.frame} or \code{sf}) if length(layers) == 1.}

\item{...}{Arguments to pass to `terra::vect()`.}
}
\value{
An \code{sf} or \code{data.frame} object.
}
\description{
This function calls Rocky Mountain Research Station (zip files) to get Geomorphic Road Analysis and Inventory Package Lite (GRAIP-Lite) data. These
datasets contain both spatial and non-spatial data in the form of a File Geodatabase (GDB).
}
\details{
Road Density, Proximity and Erosion Data (both unit sets1) and Stability Index Information. Data is available nationally and by Forest Service Region.
The GDB's currently available:
\itemize{
\item  \strong{Region 1}
\item  \strong{Region 2}
\item \strong{Region 3}
\item \strong{Region 4}
\item \strong{Region 5}
\item \strong{Region 6}
\item  \strong{Region 8}
\item  \strong{Region 9}
\item \strong{Region 10}
\item \strong{National Slope Stability}
}
}
\note{
Please use \code{\link{get_GL_layers}} to get the layer id information needed for the layer argument. This will
help with joining \code{sf} and \code{data.frame} objects. Proj4 = '+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'.
}
\examples{
\dontrun{

# get R1 GRAIP-Lite
GL_R1 <- get_GL('R1')

# get multiple layers in a list

GL_R1_multiple <- get_GL(gdb = 'R1',
layers = c('WCATT_HUCs_GL_Data_USC_Units_R01', 'WCATT_HUCs_GL_Data_SI_Units_R01'))

# Or run with a SQL query
r1_gl <- get_GL(gdb = 'R1', layers = 'WCATT_HUCs_GL_Data_USC_Units_R01',
query = "select * from \"WCATT_HUCs_GL_Data_USC_Units_R01\" where STATES='CN,MT' OR STATES='MT' AND SUBSTR(HUC_12, 1, 4) = '1701'")

}


}
\seealso{
`get_GL_layers()`
}
\author{
Josh Erickson
}
