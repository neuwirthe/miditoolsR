#' Midi structures
#'
#' @description
#' structures representing midi
#'
#' * `make_midi_df()`: make midi_df object from tracks
#' @param ... multiple dataframes containing midi structures
#' @param track track number
#' @param channel channel number
#' @param division for setting playing speed
#' @param midi_df midi_df dataframe to be converted to csv
#' @param midi_csv csv file to contain midi data to be played
#'
#' @return dataframe containing midi structures
#'
#' @name midi_structures
#'
options(scipen=999)

add_class <- function(object, new_class) {
  if (!(new_class %in% class(object))) {
    class(object) <- c(new_class, class(object))
  }
  return(object)
}

remove_class <- function(object, class) {
  class(object) <- setdiff(class(object), class)
  return(object)
}

track_start <- function(track = 1, tempo = 100000) {
  bind_rows(
    tibble(
      track = track,
      time = 0,
      command = "Start_track"
    ),
    tibble(
      track = track,
      time = 0,
      command = "Tempo",
      p1 = tempo
    )
  )
}

track_end <- function(track = 1, end_time = 250) {
  tibble(
    track = track,
    time = end_time,
    command = "End_track"
  )
}

#' @export
#' @rdname midi_structures
make_midi_track <- function(..., track = 1, channel = NA) {
  data <- list(...)
  data <- data |> keep(is.data.frame)
  track_channel <- channel
  track_no <- track
  track_data <-
    bind_rows(data) |>
    mutate(track = track_no) |>
    select(track, everything()) |>
    remove_class("midi_events") |>
    arrange("time") ->
      track_data

  if (!is.na(channel)) {
    track_data |>
      mutate(p1 = track_channel) ->
    track_data
  }

  end_time <- track_data$time |> max()

  bind_rows(
    track_start(track_no),
    track_data,
    track_end(track_no, end_time = end_time)
  ) |>
    add_class("midi_track")
}



file_header <- function(midi_type = 0, n_tracks = 1, division = 100) {
  tibble(
    track = 0,
    time = 0,
    command = "Header",
    p1 = midi_type,
    p2 = n_tracks,
    p3 = division
  )
}




file_end <- function() {
  tibble(
    track = 0,
    time = 0,
    command = "End_of_file"
  )
}

#' @export
#' @rdname midi_structures
make_midi_df <- function(..., division = 100) {
  tracks <- list(...)
  tracks_combined <-
    bind_rows(tracks) |>
    arrange("track","time")

  n_tracks <-
    tracks_combined$track |>
    unique() |>
    setdiff(0) |>
    length()

  if (n_tracks == 1) {
    midi_type <- 0
  } else {
    midi_type <- 1
  }
  bind_rows(
    file_header(midi_type = midi_type, n_tracks = n_tracks),
    tracks_combined,
    file_end()
  ) |>
    add_class("midi_df")
}


#' @export
#' @rdname midi_structures
make_midi_csv <- function(midi_df, midi_csv = NULL) {
#' @import midicsvR
#' @importFrom utils tail
#' @importFrom utils write.csv
#' @importFrom readr write_lines
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim

  old_scipen <- getOption("scipen")
  csv_con <- textConnection("csv_text", "w")
  options(scipen=999)
  write.csv(midi_df, file = csv_con, row.names = FALSE) -> forget
  rm(forget)
  options(scipen=old_scipen)
  close(csv_con)
  csv_text <- tail(csv_text, -1)

  csv_con <- textConnection(csv_text)
#  csv_file <- tempfile(fileext = ".csv")
  if(is.null(midi_csv)){
  midi_csv <- tempfile(fileext = ".csv")
  }
  readLines(csv_con)  |>
    str_remove_all('"') |>
    remove_na()  |>
    str_replace_all(",", ", ") |>
    str_replace_all("[[:space:]]", " ")  |>
   writeLines(con = midi_csv) -> forget
  rm(forget)
  midi_csv |>
    invisible()
}

#' @export
#' @rdname midi_structures
make_midi_events <- function(...){
  data <- list(...)
  bind_rows(data) |>
    add_class("midi_events")
}

remove_na <- function(text) {
  str_remove_all(text, ",[[:space:]]*NA") |>
    str_trim()
}



notes <-
  bind_rows(
  instrument(0,1,1),
  note(0,1,60,1,250),
  note(250,1,64,1,500),
)

events <-
  make_midi_events(
instrument(0,1,1),
note(0,1,60,1,250),
note(250,1,64,1,500)
)
