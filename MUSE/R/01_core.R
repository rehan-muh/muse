## Internal dependency-light helpers.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

.is_missing_scalar <- function(x, blank = TRUE) {
  is.null(x) ||
    length(x) == 0L ||
    is.na(x[[1L]]) ||
    (blank && is.character(x) && !nzchar(trimws(x[[1L]])))
}

.value_or <- function(x, default, blank = TRUE) {
  if (.is_missing_scalar(x, blank = blank)) default else x[[1L]]
}

.as_num <- function(x, default = NA_real_) {
  if (.is_missing_scalar(x)) return(default)
  out <- suppressWarnings(as.numeric(x[[1L]]))
  if (length(out) == 0L || is.na(out)) default else out
}

.as_beats <- function(x, default = NA_real_) {
  if (.is_missing_scalar(x)) return(default)
  pieces <- strsplit(gsub("\\s+", "", as.character(x[[1L]])), "\\+")[[1L]]
  values <- suppressWarnings(as.numeric(pieces))
  if (!length(values) || anyNA(values)) default else sum(values)
}

.as_int <- function(x, default = NA_integer_) {
  out <- .as_num(x, default = default)
  if (is.na(out)) default else as.integer(out)
}

.xml_text1 <- function(node, xpath, default = NA_character_, trim = TRUE) {
  hit <- xml2::xml_find_first(node, xpath)
  if (inherits(hit, "xml_missing")) return(default)
  txt <- xml2::xml_text(hit)
  if (!length(txt)) return(default)
  if (trim) txt <- trimws(txt)
  if (!nzchar(txt)) default else txt
}

.xml_has <- function(node, xpath) {
  !inherits(xml2::xml_find_first(node, xpath), "xml_missing")
}

.require_columns <- function(x, columns, name = deparse(substitute(x))) {
  if (!is.data.frame(x)) {
    stop("`", name, "` must be a data.frame.", call. = FALSE)
  }
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    stop(
      "`", name, "` is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rbind_fill <- function(dfs, template = NULL) {
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  dfs <- dfs[vapply(dfs, is.data.frame, logical(1))]
  dfs <- dfs[vapply(dfs, nrow, integer(1)) > 0L]
  if (!length(dfs)) {
    return(if (is.null(template)) data.frame() else template[0, , drop = FALSE])
  }

  cols <- unique(c(
    if (!is.null(template)) names(template),
    unlist(lapply(dfs, names), use.names = FALSE)
  ))
  dfs <- lapply(dfs, function(d) {
    missing <- setdiff(cols, names(d))
    for (nm in missing) {
      if (!is.null(template) && nm %in% names(template)) {
        d[[nm]] <- template[[nm]][NA_integer_]
      } else {
        d[[nm]] <- NA
      }
    }
    d[cols]
  })

  out <- do.call(rbind, dfs)
  rownames(out) <- NULL
  out
}

.need <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf(
        "Package '%s' is required for %s but is not installed.\n  install.packages(\"%s\")",
        pkg,
        what,
        pkg
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.with_seed <- function(seed, code) {
  if (is.null(seed)) return(force(code))

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  force(code)
}
## Typed schemas used by parsers, validators, and downstream joins.

.empty_events <- function() data.frame(
  event_id = integer(), part_id = character(), voice = character(), staff = character(),
  measure = integer(), measure_index = integer(), measure_label = character(), seq = integer(),
  onset_div = numeric(), onset_q = numeric(), measure_onset_q = numeric(), score_onset_q = numeric(),
  duration_div = numeric(), duration_q = numeric(), score_offset_q = numeric(), onset_beat = numeric(),
  beat_strength = numeric(), is_rest = logical(), is_grace = logical(), is_chord = logical(),
  is_unpitched = logical(), duration_missing = logical(), tie_start = logical(), tie_stop = logical(),
  midi = numeric(), step = character(), octave = numeric(),
  alter = numeric(), pitch_class = integer(), pitch_class_12 = numeric(), pc_name = character(),
  interval = numeric(), contour = character(), stringsAsFactors = FALSE
)

.empty_lyrics <- function() data.frame(
  event_id = integer(), part_id = character(), voice = character(), staff = character(),
  verse = character(), lyric_name = character(), syllabic = character(), text = character(),
  text_raw = character(), text_present = logical(), has_elision = logical(),
  has_extend = logical(), extend_type = character(), stringsAsFactors = FALSE
)

.empty_syllables <- function() data.frame(
  syllable_id = integer(), part_id = character(), voice = character(), staff = character(),
  verse = character(), word_id = integer(), word_uid = character(), word_index = integer(),
  syl_in_word = integer(), syllabic = character(), text = character(), onset_event_id = integer(),
  n_notes = integer(), n_attacks = integer(), dur_q = numeric(), is_melisma = logical(),
  alignment_policy = character(), stringsAsFactors = FALSE
)

.empty_alignment <- function() data.frame(
  event_id = integer(), syllable_id = integer(), part_id = character(), voice = character(),
  verse = character(), melisma_pos = integer(), explicit = logical(), stringsAsFactors = FALSE
)

.empty_measures <- function() data.frame(
  part_id = character(), measure = integer(), measure_index = integer(), measure_label = character(),
  divisions = numeric(), beats = numeric(), beat_type = numeric(),
  time_signature = character(), meter_supported = logical(), fifths = numeric(),
  implicit = logical(), measure_onset_q = numeric(), actual_duration_q = numeric(),
  nominal_duration_q = numeric(), stringsAsFactors = FALSE
)

.empty_parts <- function() data.frame(part_id = character(), part_name = character(), stringsAsFactors = FALSE)
#' Construct a transparent MUSE score
#' @param events,lyrics,syllables,measures,parts Relational data.frames.
#' @param meta Score-level metadata list; alignment is stored in
#'   `meta$alignment`.
#' @param validate Validate and stop on schema errors.
#' @return A `muse_score`.
#' @export
new_muse_score <- function(
    events = .empty_events(),
    lyrics = .empty_lyrics(),
    syllables = .empty_syllables(),
    measures = .empty_measures(),
    parts = .empty_parts(),
    meta = list(),
    validate = TRUE) {
  score <- structure(
    list(
      events = events,
      lyrics = lyrics,
      syllables = syllables,
      measures = measures,
      parts = parts,
      meta = meta
    ),
    class = "muse_score"
  )
  if (validate) validate_muse_score(score, strict = TRUE)
  score
}

#' Test whether an object is a MUSE score
#' @param x Any object.
#' @return Logical scalar.
#' @export
is_muse_score <- function(x) {
  inherits(x, "muse_score")
}

#' @export
print.muse_score <- function(x, ...) {
  cat("<muse_score>\n")
  cat(sprintf("  title : %s\n", .value_or(x$meta$title, "(untitled)")))
  cat(sprintf(
    "  source: %s\n",
    basename(.value_or(x$meta$source, "(in-memory)"))
  ))
  cat(sprintf("  parts : %d\n", nrow(x$parts)))

  sung <- if (nrow(x$events)) {
    sum(!x$events$is_rest & !x$events$is_grace, na.rm = TRUE)
  } else {
    0L
  }
  cat(sprintf(
    "  events: %d (%d sung notes)\n",
    nrow(x$events),
    sung
  ))

  melismas <- if (nrow(x$syllables)) {
    sum(x$syllables$is_melisma, na.rm = TRUE)
  } else {
    0L
  }
  cat(sprintf(
    "  lyrics: %d rows, %d syllables, %d melismas\n",
    nrow(x$lyrics),
    nrow(x$syllables),
    melismas
  ))
  if (nrow(x$lyrics)) {
    cat(sprintf(
      "  verses: %s\n",
      paste(sort(unique(x$lyrics$verse)), collapse = ", ")
    ))
  }
  invisible(x)
}

#' @export
summary.muse_score <- function(object, ...) {
  events <- object$events
  sung <- events[!events$is_rest & !events$is_grace, , drop = FALSE]
  measure_counts <- if (nrow(object$measures)) {
    table(object$measures$part_id)
  } else {
    integer()
  }

  out <- list(
    parts = nrow(object$parts),
    measures = if (length(measure_counts)) max(measure_counts) else 0L,
    part_measure_rows = nrow(object$measures),
    events = nrow(events),
    sung_notes = nrow(sung),
    rests = sum(events$is_rest, na.rm = TRUE),
    grace_notes = sum(events$is_grace, na.rm = TRUE),
    midi_range = if (nrow(sung) && any(is.finite(sung$midi))) {
      range(sung$midi[is.finite(sung$midi)])
    } else {
      c(NA_real_, NA_real_)
    },
    syllables = nrow(object$syllables),
    melismas = if (nrow(object$syllables)) {
      sum(object$syllables$is_melisma, na.rm = TRUE)
    } else {
      0L
    },
    max_melisma = if (nrow(object$syllables)) {
      max(object$syllables$n_notes, na.rm = TRUE)
    } else {
      0L
    },
    problems = nrow(validate_muse_score(object))
  )
  class(out) <- "summary.muse_score"
  out
}

#' @export
print.summary.muse_score <- function(x, ...) {
  cat("muse_score summary\n")
  cat(sprintf(
    "  parts ............ %d\n  measures/part ..... %d\n  part-measure rows . %d\n  events ........... %d\n",
    x$parts,
    x$measures,
    x$part_measure_rows,
    x$events
  ))
  cat(sprintf(
    "  sung notes ....... %d\n  rests ............ %d\n  grace notes ...... %d\n",
    x$sung_notes,
    x$rests,
    x$grace_notes
  ))
  cat(sprintf("  MIDI range ....... %s\n", paste(x$midi_range, collapse = "-")))
  cat(sprintf(
    "  syllables ........ %d\n  melismas ......... %d (longest %d notes)\n",
    x$syllables,
    x$melismas,
    x$max_melisma
  ))
  cat(sprintf("  validation issues  %d\n", x$problems))
  invisible(x)
}

#' @export
as.data.frame.muse_score <- function(x, ..., level = "event") {
  muse_table(x, level = level)
}
#' Convert spelled pitch to MIDI number
#' @param step Note letter A-G.
#' @param octave Scientific-pitch octave.
#' @param alter Chromatic alteration in semitones; fractional values are kept.
#' @return Numeric MIDI number; C4 = 60.
#' @export
pitch_to_midi <- function(step, octave, alter = 0) {
  n <- max(length(step), length(octave), length(alter))
  if (!is.finite(n) || n == 0L) return(numeric())

  base <- c(C = 0, D = 2, E = 4, F = 5, G = 7, A = 9, B = 11)
  step <- rep_len(toupper(as.character(step)), n)
  octave <- rep_len(suppressWarnings(as.numeric(octave)), n)
  alter <- rep_len(suppressWarnings(as.numeric(alter)), n)
  base_value <- unname(base[step])

  out <- (octave + 1) * 12 + base_value + alter
  out[is.na(step) | is.na(octave) | is.na(alter) | is.na(base_value)] <-
    NA_real_
  out
}

#' Convert MIDI number to an integer 12-TET pitch class
#' @param midi Numeric MIDI value.
#' @param tolerance Distance from an integer accepted as 12-TET.
#' @return Integer 0-11, or `NA` for microtonal pitches.
#' @export
midi_to_pc <- function(midi, tolerance = 1e-9) {
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) {
    stop("`tolerance` must be one finite nonnegative number.", call. = FALSE)
  }
  value <- suppressWarnings(as.numeric(midi)) %% 12
  integer_like <- !is.na(value) & abs(value - round(value)) <= tolerance
  out <- rep(NA_integer_, length(value))
  out[integer_like] <- as.integer(round(value[integer_like])) %% 12L
  out
}

#' Name a 12-TET pitch class
#' @param pc Integer pitch class.
#' @param flats Use flat rather than sharp labels.
#' @return Character pitch-class name; microtonal/noninteger values are `NA`.
#' @export
pc_name <- function(pc, flats = FALSE) {
  sharp <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  flat <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")
  value <- suppressWarnings(as.numeric(pc))
  integer_like <- !is.na(value) & abs(value - round(value)) < 1e-9
  out <- rep(NA_character_, length(value))
  labels <- if (isTRUE(flats)) flat else sharp
  out[integer_like] <- labels[
    (as.integer(round(value[integer_like])) %% 12L) + 1L
  ]
  out
}

#' Add melodic intervals and contour by part and voice
#'
#' Simultaneous chord members are retained in document sequence. For genuinely
#' polyphonic material, users should define a melodic-voice selection before
#' interpreting these intervals as a monophonic contour.
#'
#' @param events Event table.
#' @return Event table with `interval` and `contour`.
#' @export
melodic_intervals <- function(events) {
  .require_columns(
    events,
    c("part_id", "voice", "seq", "is_rest", "is_grace", "midi"),
    "events"
  )
  events$interval <- NA_real_
  events$contour <- NA_character_
  if (!nrow(events)) return(events)

  pitched <- !events$is_rest & !events$is_grace & is.finite(events$midi)
  group <- paste(events$part_id, events$voice, sep = "\r")
  for (group_value in unique(group)) {
    indices <- which(group == group_value & pitched)
    order_index <- if ("score_onset_q" %in% names(events)) {
      order(events$score_onset_q[indices], events$seq[indices])
    } else {
      order(events$seq[indices])
    }
    indices <- indices[order_index]
    if (length(indices) < 2L) next

    interval <- diff(events$midi[indices])
    events$interval[indices[-1L]] <- interval
    events$contour[indices[-1L]] <- ifelse(
      interval > 0,
      "up",
      ifelse(interval < 0, "down", "same")
    )
  }
  events
}
#' Convert quarter-note onsets to notated beat positions
#' @param onset_q Numeric onset within a measure in quarter notes.
#' @param beat_type Time-signature denominator.
#' @return Zero-based notated beat position, or `NA` for invalid denominators.
#' @export
onset_to_beat <- function(onset_q, beat_type) {
  n <- max(length(onset_q), length(beat_type))
  onset_q <- rep_len(as.numeric(onset_q), n)
  beat_type <- rep_len(as.numeric(beat_type), n)
  out <- rep(NA_real_, n)
  ok <- is.finite(onset_q) & is.finite(beat_type) & beat_type > 0
  out[ok] <- onset_q[ok] / (4 / beat_type[ok])
  out
}

#' Transparent metrical-strength heuristic
#'
#' Compound x/8 meters whose numerator is divisible by three are grouped into
#' dotted-quarter beats. The measure downbeat receives strength 1, an exact
#' halfway group/beat receives 0.75, other group/beat onsets receive 0.5, and
#' subdivisions receive 0.25. This is a documented baseline rather than a
#' universal theory of metrical structure; additive-meter groupings are not
#' recoverable from a summed MusicXML numerator alone.
#'
#' @param onset_beat Zero-based notated beat position.
#' @param beats Numerator of the time signature.
#' @param beat_type Denominator of the time signature.
#' @return Numeric strength in `{0.25, 0.5, 0.75, 1}`.
#' @export
metric_strength <- function(onset_beat, beats = 4, beat_type = 4) {
  n <- max(length(onset_beat), length(beats), length(beat_type))
  onset_beat <- rep_len(as.numeric(onset_beat), n)
  beats <- rep_len(as.numeric(beats), n)
  beat_type <- rep_len(as.numeric(beat_type), n)

  out <- rep(NA_real_, n)
  ok <- is.finite(onset_beat) & is.finite(beats) & beats > 0 &
    is.finite(beat_type) & beat_type > 0
  if (!any(ok)) return(out)

  compound <- beats > 3 & beats %% 3 == 0 & beat_type == 8
  group_size <- ifelse(compound, 3, 1)
  group_pos <- onset_beat / group_size
  groups_per_measure <- beats / group_size
  phase <- group_pos %% groups_per_measure
  on_group <- abs(group_pos - round(group_pos)) < 1e-9
  downbeat <- abs(phase) < 1e-9
  halfway <- on_group & groups_per_measure >= 2 &
    abs(phase - groups_per_measure / 2) < 1e-9

  out[ok] <- 0.25
  out[ok & on_group] <- 0.5
  out[ok & halfway] <- 0.75
  out[ok & downbeat] <- 1
  out
}
