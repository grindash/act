% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_img.R
\name{add_img}
\alias{add_img}
\title{Add and image on a plot and keep the ratio of a picture.
Credit: https://stackoverflow.com/a/56018973}
\usage{
add_img(
  img,
  x = 0,
  y = 0,
  width = 3,
  decalageX = 0.5,
  decalageY = 0.5,
  interpolate = FALSE
)
}
\arguments{
\item{img}{picture to import}

\item{x}{position}

\item{y}{position}

\item{width}{picture width}

\item{decalageX}{change the position reference. 0 = right, 1 = left, 0.5 = center}

\item{decalageY}{change the position reference. 0 = bottom, 1 = top, 0.5 = center}

\item{interpolate}{osef}
}
\value{
draw an image
}
\description{
Add and image on a plot and keep the ratio of a picture.
Credit: https://stackoverflow.com/a/56018973
}
