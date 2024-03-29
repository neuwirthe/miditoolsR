% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/instruments.R
\name{instruments}
\alias{instruments}
\alias{instrument}
\alias{bank_select}
\alias{drum_bank_select}
\alias{bank_select_gm2}
\title{instruments and drums}
\usage{
instrument(time, channel = 1, instrument = 1)

bank_select(time, channel, bank)

drum_bank_select(time, channel, bank)

bank_select_gm2(time, channel, bank)
}
\arguments{
\item{time}{starting time of note or time of changing instrument in milliseconds}

\item{channel}{channel to use on midi output, in range 1-16,
channel 10 is the drum channel.}

\item{instrument}{instrument number, range 1 to 128}

\item{bank}{instrument or drum bank to select}
}
\description{
Instruments and drums
\itemize{
\item \code{instrument()}: selects instrument to be played, differemt channels
can play different instruments simultaneously
\item \code{bank_select()}: changes set of instruments available
(offers variations of standard instruments)
\item \code{bank_select_gm2()}: changes set of instruments available
(offers variations of standard instruments)
if midiplayer supports MIDI standard GM2
\item \code{drum_bank_select()}: changes drum set
}
}
