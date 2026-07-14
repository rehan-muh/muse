## Pure lyric-to-note alignment. No XML objects enter this layer.

#' Build syllables and note-to-syllable alignment
#'
#' `hybrid` chooses explicit extension semantics for a part/voice/staff/verse
#' group when that group contains any MusicXML `<extend>` markup; otherwise it
#' applies the legacy blank-note convention. The selected policy is stored on
#' every derived syllable for sensitivity analysis.
#'
#' @param events Event table. At minimum: `event_id`, `part_id`, `voice`,
#'   `staff`, `seq`, `is_rest`, `is_grace`, and `duration_q`.
#' @param lyrics Lyric table. At minimum: `event_id`, `part_id`, `voice`,
#'   `staff`, `verse`, `syllabic`, and `text`.
#' @param melisma_policy `"explicit"`, `"hybrid"`, or `"legacy"`.
#' @return A list with `syllables` and `alignment` data.frames.
#' @export
build_syllables <- function(
    events,
    lyrics,
    melisma_policy = c("hybrid", "explicit", "legacy")) {
  melisma_policy <- match.arg(melisma_policy)
  if (!is.data.frame(events) || !is.data.frame(lyrics)) {
    stop("`events` and `lyrics` must be data.frames.", call. = FALSE)
  }
  if (!nrow(events) || !nrow(lyrics)) {
    return(list(
      syllables = .empty_syllables(),
      alignment = .empty_alignment()
    ))
  }

  .require_columns(
    events,
    c("event_id", "part_id", "voice", "staff", "seq", "is_rest", "is_grace", "duration_q"),
    "events"
  )
  .require_columns(
    lyrics,
    c("event_id", "part_id", "voice", "staff", "verse", "syllabic", "text"),
    "lyrics"
  )

  ev <- events
  if (!"tie_stop" %in% names(ev)) ev$tie_stop <- FALSE

  ly <- lyrics
  if (!"text_present" %in% names(ly)) {
    ly$text_present <- !is.na(ly$text) & nzchar(trimws(ly$text))
  }
  if (!"has_extend" %in% names(ly)) ly$has_extend <- FALSE
  if (!"extend_type" %in% names(ly)) ly$extend_type <- NA_character_
  ly$text <- ifelse(ly$text_present, trimws(ly$text), NA_character_)
  ly$syllabic <- tolower(ly$syllabic)
  ly$syllabic[is.na(ly$syllabic) & ly$text_present] <- "single"

  lyric_key <- paste(
    ly$event_id,
    ly$part_id,
    ly$voice,
    ly$staff,
    ly$verse,
    sep = "\r"
  )
  if (anyDuplicated(lyric_key)) {
    stop(
      "More than one lyric row has the same event/part/voice/staff/verse key.",
      call. = FALSE
    )
  }

  syllable_rows <- list()
  alignment_rows <- list()
  syllable_counter <- 0L
  word_counter <- 0L
  groups <- unique(ly[, c("part_id", "voice", "staff", "verse"), drop = FALSE])

  for (group_index in seq_len(nrow(groups))) {
    group <- groups[group_index, , drop = FALSE]
    event_group <- ev[
      ev$part_id == group$part_id &
        ev$voice == group$voice &
        ev$staff == group$staff,
      ,
      drop = FALSE
    ]
    event_group <- event_group[order(event_group$seq), , drop = FALSE]
    lyric_group <- ly[
      ly$part_id == group$part_id &
        ly$voice == group$voice &
        ly$staff == group$staff &
        ly$verse == group$verse,
      ,
      drop = FALSE
    ]
    if (!nrow(event_group) || !nrow(lyric_group)) next

    lyrics_by_event <- split(lyric_group, lyric_group$event_id)
    policy <- if (melisma_policy == "hybrid") {
      if (any(lyric_group$has_extend, na.rm = TRUE)) "explicit" else "legacy"
    } else {
      melisma_policy
    }

    current <- NULL
    current_open <- FALSE
    word_index <- 0L
    syllable_in_word <- 0L

    add_continuation <- function(event_row, current_index, extension_marked) {
      syllable_row <- syllable_rows[[current_index]]
      syllable_row$n_notes <- syllable_row$n_notes + 1L
      syllable_row$n_attacks <- syllable_row$n_attacks +
        as.integer(!isTRUE(event_row$tie_stop))
      syllable_row$dur_q <- syllable_row$dur_q +
        .value_or(event_row$duration_q, 0, blank = FALSE)
      syllable_row$is_melisma <- syllable_row$n_notes > 1L
      syllable_rows[[current_index]] <<- syllable_row

      alignment_rows[[length(alignment_rows) + 1L]] <<- data.frame(
        event_id = event_row$event_id,
        syllable_id = syllable_row$syllable_id,
        part_id = group$part_id,
        voice = group$voice,
        verse = as.character(group$verse),
        melisma_pos = syllable_row$n_notes,
        explicit = extension_marked,
        stringsAsFactors = FALSE
      )
    }

    for (event_index in seq_len(nrow(event_group))) {
      event_row <- event_group[event_index, , drop = FALSE]
      if (isTRUE(event_row$is_rest)) {
        current <- NULL
        current_open <- FALSE
        next
      }
      if (isTRUE(event_row$is_grace)) next

      lyric_row <- lyrics_by_event[[as.character(event_row$event_id)]]
      text_row <- if (!is.null(lyric_row)) {
        which(lyric_row$text_present %in% TRUE)[1L]
      } else {
        NA_integer_
      }

      if (!is.na(text_row)) {
        lyric_text_row <- lyric_row[text_row, , drop = FALSE]
        syllabic <- .value_or(lyric_text_row$syllabic, "single")
        if (!syllabic %in% c("single", "begin", "middle", "end")) {
          warning(
            "Unknown syllabic value '", syllabic, "'; treating it as 'single'.",
            call. = FALSE
          )
          syllabic <- "single"
        }

        if (syllabic %in% c("single", "begin") || word_index == 0L) {
          word_index <- word_index + 1L
          word_counter <- word_counter + 1L
          syllable_in_word <- 1L
        } else {
          syllable_in_word <- syllable_in_word + 1L
        }

        syllable_counter <- syllable_counter + 1L
        word_uid <- paste(
          group$part_id,
          group$voice,
          group$staff,
          group$verse,
          word_index,
          sep = ":"
        )
        syllable_rows[[length(syllable_rows) + 1L]] <- data.frame(
          syllable_id = syllable_counter,
          part_id = group$part_id,
          voice = group$voice,
          staff = group$staff,
          verse = as.character(group$verse),
          word_id = word_counter,
          word_uid = word_uid,
          word_index = word_index,
          syl_in_word = syllable_in_word,
          syllabic = syllabic,
          text = lyric_text_row$text,
          onset_event_id = event_row$event_id,
          n_notes = 1L,
          n_attacks = as.integer(!isTRUE(event_row$tie_stop)),
          dur_q = .value_or(event_row$duration_q, NA_real_, blank = FALSE),
          is_melisma = FALSE,
          alignment_policy = policy,
          stringsAsFactors = FALSE
        )
        current <- length(syllable_rows)
        alignment_rows[[length(alignment_rows) + 1L]] <- data.frame(
          event_id = event_row$event_id,
          syllable_id = syllable_counter,
          part_id = group$part_id,
          voice = group$voice,
          verse = as.character(group$verse),
          melisma_pos = 1L,
          explicit = isTRUE(lyric_text_row$has_extend),
          stringsAsFactors = FALSE
        )

        current_open <- if (policy == "legacy") {
          TRUE
        } else {
          isTRUE(lyric_text_row$has_extend) &&
            !identical(
              .value_or(lyric_text_row$extend_type, "unspecified"),
              "stop"
            )
        }
        next
      }

      if (is.null(current)) next
      if (policy == "legacy") {
        add_continuation(event_row, current, extension_marked = FALSE)
        next
      }

      blank_extend <- !is.null(lyric_row) && any(lyric_row$has_extend %in% TRUE)
      if (current_open || blank_extend) {
        add_continuation(event_row, current, extension_marked = TRUE)
        if (blank_extend) {
          types <- lyric_row$extend_type[lyric_row$has_extend %in% TRUE]
          current_open <- !"stop" %in% types
        }
      }
    }
  }

  list(
    syllables = .rbind_fill(syllable_rows, .empty_syllables()),
    alignment = .rbind_fill(alignment_rows, .empty_alignment())
  )
}
