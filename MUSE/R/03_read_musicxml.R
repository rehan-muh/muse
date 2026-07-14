## Public MusicXML reader. XML-specific work is delegated to small internal
## functions so parsing, alignment, validation, and analysis remain separable.

#' Read a MusicXML score
#' @param path Path to `.xml`, `.musicxml`, or compressed `.mxl`.
#' @param flats Use flat pitch-class spellings for 12-TET pitches.
#' @param melisma_policy One of `"explicit"`, `"hybrid"`, or `"legacy"`.
#'   `explicit` only extends lyrics when MusicXML marks an `<extend>`;
#'   `hybrid` uses explicit markup when present and the legacy blank-note
#'   convention otherwise; `legacy` treats every following lyricless note as a
#'   continuation until a rest or new lyric.
#' @param validate Validate the resulting relational tables.
#' @return A `muse_score`.
#' @export
read_musicxml <- function(path, flats = FALSE,
                          melisma_policy = c("hybrid", "explicit", "legacy"),
                          validate = TRUE) {
  melisma_policy <- match.arg(melisma_policy)
  if (length(path) != 1L || !file.exists(path)) stop("File not found: ", path, call. = FALSE)
  doc <- if (grepl("\\.mxl$", path, ignore.case = TRUE)) {
    .read_mxl(path)
  } else {
    .read_xml_safely(path)
  }
  .parse_score(doc, source = normalizePath(path, winslash = "/", mustWork = FALSE),
               flats = flats, melisma_policy = melisma_policy, validate = validate)
}

#' Read MusicXML from a string
#' @param text Length-one MusicXML string.
#' @inheritParams read_musicxml
#' @return A `muse_score`.
#' @export
read_musicxml_string <- function(text, flats = FALSE,
                                 melisma_policy = c("hybrid", "explicit", "legacy"),
                                 validate = TRUE) {
  melisma_policy <- match.arg(melisma_policy)
  if (!is.character(text) || length(text) != 1L) stop("`text` must be one character string.", call. = FALSE)
  .parse_score(.read_xml_safely(text), source = "(string)", flats = flats,
               melisma_policy = melisma_policy, validate = validate)
}

.read_mxl <- function(path) {
  tmp <- tempfile("muse-mxl-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  listing <- utils::unzip(path, list = TRUE)
  member <- gsub("\\\\", "/", listing$Name)
  if (any(grepl("^/|(^|/)\\.\\.(/|$)", member)))
    stop("Unsafe path found in MXL archive.", call. = FALSE)
  if ("Length" %in% names(listing) && sum(listing$Length, na.rm = TRUE) > 250 * 1024^2)
    stop("MXL archive exceeds the 250 MiB extraction limit.", call. = FALSE)
  utils::unzip(path, exdir = tmp)
  container <- file.path(tmp, "META-INF", "container.xml")
  rel <- NA_character_
  if (file.exists(container)) {
    cdoc <- xml2::xml_ns_strip(.read_xml_safely(container))
    rf <- xml2::xml_find_first(cdoc, ".//rootfile")
    if (!inherits(rf, "xml_missing")) rel <- xml2::xml_attr(rf, "full-path")
  }
  if (!is.na(rel)) {
    rel <- gsub("\\\\", "/", rel)
    if (grepl("^/|(^|/)\\.\\.(/|$)", rel)) stop("Unsafe rootfile path in MXL archive.", call. = FALSE)
    target <- file.path(tmp, rel)
  } else {
    xmls <- list.files(tmp, pattern = "\\.(xml|musicxml)$", recursive = TRUE, full.names = TRUE)
    xmls <- xmls[!grepl("META-INF[/\\\\]container\\.xml$", xmls, ignore.case = TRUE)]
    if (!length(xmls)) stop("No score XML found inside MXL archive.", call. = FALSE)
    target <- xmls[[1L]]
  }
  if (!file.exists(target)) stop("MXL rootfile is missing: ", rel, call. = FALSE)
  .read_xml_safely(target)
}

.read_xml_safely <- function(x) {
  xml2::read_xml(x, options = c("NOBLANKS", "NONET"))
}

.parse_score <- function(doc, source, flats, melisma_policy, validate) {
  doc <- xml2::xml_ns_strip(doc)
  root <- xml2::xml_root(doc)
  rname <- xml2::xml_name(root)
  if (identical(rname, "score-timewise"))
    stop("score-timewise is not supported; convert to score-partwise before import.", call. = FALSE)
  if (!identical(rname, "score-partwise"))
    stop("Root element must be 'score-partwise'; found '", rname, "'.", call. = FALSE)

  title <- .xml_text1(root, "work/work-title")
  title <- .value_or(title, .xml_text1(root, "movement-title"))
  creator_nodes <- xml2::xml_find_all(root, "identification/creator")
  creators <- if (length(creator_nodes)) {
    creator_types <- xml2::xml_attr(creator_nodes, "type")
    creator_types[is.na(creator_types) | !nzchar(creator_types)] <- "creator"
    stats::setNames(trimws(xml2::xml_text(creator_nodes)), make.unique(creator_types))
  } else character()
  meta <- list(
    source = source,
    title = .value_or(title, basename(source)),
    movement_number = .xml_text1(root, "movement-number"),
    software = .xml_text1(root, "identification/encoding/software"),
    rights = .xml_text1(root, "identification/rights"),
    relation = .xml_text1(root, "identification/relation"),
    creators = creators,
    melisma_policy = melisma_policy,
    parser_version = "0.2.0"
  )

  sp <- xml2::xml_find_all(root, "part-list/score-part")
  parts <- if (length(sp)) data.frame(
    part_id = as.character(xml2::xml_attr(sp, "id")),
    part_name = vapply(sp, .xml_text1, character(1), xpath = "part-name"),
    stringsAsFactors = FALSE
  ) else .empty_parts()
  part_nodes <- xml2::xml_find_all(root, "part")
  actual_ids <- vapply(seq_along(part_nodes), function(i) {
    .value_or(xml2::xml_attr(part_nodes[[i]], "id"), paste0("P", i))
  }, character(1))
  missing_parts <- setdiff(actual_ids, parts$part_id)
  if (length(missing_parts)) {
    parts <- .rbind_fill(list(
      parts,
      data.frame(part_id = missing_parts, part_name = NA_character_, stringsAsFactors = FALSE)
    ), .empty_parts())
  }

  ev <- list(); ly <- list(); me <- list(); offset <- 0L
  for (pn in part_nodes) {
    pid <- .value_or(xml2::xml_attr(pn, "id"), paste0("P", length(ev) + 1L))
    res <- .extract_part(pn, pid, offset, flats)
    ev[[length(ev) + 1L]] <- res$events
    ly[[length(ly) + 1L]] <- res$lyrics
    me[[length(me) + 1L]] <- res$measures
    offset <- offset + res$n_events
  }
  events <- .rbind_fill(ev, .empty_events())
  lyrics <- .rbind_fill(ly, .empty_lyrics())
  measures <- .rbind_fill(me, .empty_measures())
  if (nrow(events)) events <- melodic_intervals(events)
  built <- build_syllables(events, lyrics, melisma_policy = melisma_policy)
  meta$alignment <- built$alignment
  score <- new_muse_score(events, lyrics, built$syllables, measures, parts, meta, validate = FALSE)
  if (validate) validate_muse_score(score, strict = TRUE)
  score
}

.lyric_text <- function(lyric_node) {
  children <- xml2::xml_children(lyric_node)
  keep <- xml2::xml_name(children) %in% c("text", "elision")
  children <- children[keep]
  if (!length(children)) return(NA_character_)
  pieces <- vapply(children, function(node) {
    value <- xml2::xml_text(node)
    if (xml2::xml_name(node) == "elision" && !nzchar(value)) " " else value
  }, character(1))
  paste0(pieces, collapse = "")
}

.read_time_signature <- function(attributes_node, current) {
  time_node <- xml2::xml_find_first(attributes_node, "time")
  if (inherits(time_node, "xml_missing")) return(current)
  if (.xml_has(time_node, "senza-misura")) {
    return(list(
      beats = NA_real_,
      beat_type = NA_real_,
      time_signature = "senza-misura",
      meter_supported = FALSE,
      nominal_duration_q = NA_real_
    ))
  }

  beat_nodes <- xml2::xml_find_all(time_node, "beats")
  type_nodes <- xml2::xml_find_all(time_node, "beat-type")
  if (!length(beat_nodes) || !length(type_nodes)) return(current)

  beat_text <- trimws(xml2::xml_text(beat_nodes))
  type_text <- trimws(xml2::xml_text(type_nodes))
  beat_values <- vapply(beat_text, .as_beats, numeric(1))
  type_values <- suppressWarnings(as.numeric(type_text))

  if (length(type_values) == 1L && length(beat_values) > 1L) {
    type_values <- rep(type_values, length(beat_values))
  }
  pairable <- length(beat_values) == length(type_values)
  valid <- pairable &&
    all(is.finite(beat_values)) &&
    all(is.finite(type_values)) &&
    all(type_values > 0)

  signature <- if (pairable) {
    paste(paste0(beat_text, "/", type_text), collapse = "+")
  } else {
    paste0(paste(beat_text, collapse = "+"), "/", paste(type_text, collapse = "+"))
  }
  nominal <- if (valid) sum(beat_values * 4 / type_values) else NA_real_
  common_denominator <- valid && length(unique(type_values)) == 1L

  list(
    beats = if (common_denominator) sum(beat_values) else NA_real_,
    beat_type = if (common_denominator) type_values[[1L]] else NA_real_,
    time_signature = signature,
    meter_supported = common_denominator,
    nominal_duration_q = nominal
  )
}

.extract_part <- function(part_node, part_id, eid_offset, flats) {
  ev_rows <- list(); ly_rows <- list(); me_rows <- list()
  event_n <- 0L; seq_n <- 0L; measure_start_q <- 0
  divisions <- 1
  beats <- 4
  beat_type <- 4
  time_signature <- "4/4"
  meter_supported <- TRUE
  nominal_duration_q <- 4
  fifths <- NA_real_
  measure_nodes <- xml2::xml_find_all(part_node, "measure")

  for (mi in seq_along(measure_nodes)) {
    mnode <- measure_nodes[[mi]]
    label <- .value_or(xml2::xml_attr(mnode, "number"), as.character(mi))
    numeric_label <- suppressWarnings(as.integer(label))
    implicit <- identical(xml2::xml_attr(mnode, "implicit"), "yes")
    cursor_q <- 0; max_q <- 0; last_onset_by_voice <- numeric()

    for (child in xml2::xml_children(mnode)) {
      nm <- xml2::xml_name(child)
      if (nm == "attributes") {
        divisions <- .as_num(.xml_text1(child, "divisions"), divisions)
        meter <- .read_time_signature(
          child,
          list(
            beats = beats,
            beat_type = beat_type,
            time_signature = time_signature,
            meter_supported = meter_supported,
            nominal_duration_q = nominal_duration_q
          )
        )
        beats <- meter$beats
        beat_type <- meter$beat_type
        time_signature <- meter$time_signature
        meter_supported <- meter$meter_supported
        nominal_duration_q <- meter$nominal_duration_q
        if (.xml_has(child, "key")) {
          fifths <- .as_num(.xml_text1(child, "key/fifths"), NA_real_)
        }
        next
      }
      if (nm %in% c("backup", "forward")) {
        amount_q <- .as_num(.xml_text1(child, "duration"), 0) / divisions
        cursor_q <- cursor_q + if (nm == "backup") -amount_q else amount_q
        if (cursor_q < -1e-9) warning("Backup moved before measure start in part ", part_id,
                                      ", measure ", label, ".", call. = FALSE)
        cursor_q <- max(cursor_q, 0)
        max_q <- max(max_q, cursor_q)
        next
      }
      if (nm != "note") next

      event_n <- event_n + 1L; seq_n <- seq_n + 1L
      event_id <- eid_offset + event_n
      is_grace <- .xml_has(child, "grace")
      is_chord <- .xml_has(child, "chord")
      is_rest <- .xml_has(child, "rest")
      is_unpitched <- .xml_has(child, "unpitched")
      voice <- .value_or(.xml_text1(child, "voice"), "1")
      staff <- .value_or(.xml_text1(child, "staff"), "1")
      duration_missing <- !is_grace && is.na(.as_num(.xml_text1(child, "duration")))
      dur_div <- if (is_grace) 0 else .as_num(.xml_text1(child, "duration"), NA_real_)
      duration_q <- if (is.finite(divisions) && divisions > 0 && is.finite(dur_div)) dur_div / divisions else NA_real_
      onset_q <- if (is_chord && voice %in% names(last_onset_by_voice)) last_onset_by_voice[[voice]] else cursor_q
      if (!is_chord) last_onset_by_voice[[voice]] <- onset_q
      if (!is_chord && !is_grace) cursor_q <- cursor_q + .value_or(duration_q, 0, blank = FALSE)
      max_q <- max(max_q, onset_q + duration_q, cursor_q, na.rm = TRUE)

      step <- .xml_text1(child, "pitch/step")
      octave <- .as_num(.xml_text1(child, "pitch/octave"))
      alter <- .as_num(.xml_text1(child, "pitch/alter"), 0)
      midi <- if (!is_rest && !is.na(step) && !is.na(octave)) pitch_to_midi(step, octave, alter) else NA_real_
      pc12 <- if (!is.na(midi)) midi %% 12 else NA_real_
      pcint <- if (!is.na(pc12) && abs(pc12 - round(pc12)) < 1e-9) as.integer(round(pc12)) else NA_integer_
      ties <- xml2::xml_attr(xml2::xml_find_all(child, "tie"), "type")
      onset_beat <- onset_to_beat(onset_q, beat_type)

      ev_rows[[length(ev_rows) + 1L]] <- data.frame(
        event_id=event_id, part_id=part_id, voice=voice, staff=staff,
        measure=if (is.na(numeric_label)) mi else numeric_label,
        measure_index=mi, measure_label=label, seq=seq_n,
        onset_div=onset_q * divisions, onset_q=onset_q,
        measure_onset_q=measure_start_q, score_onset_q=measure_start_q + onset_q,
        duration_div=dur_div, duration_q=duration_q,
        score_offset_q=measure_start_q + onset_q + duration_q,
        onset_beat=onset_beat, beat_strength=metric_strength(onset_beat, beats, beat_type),
        is_rest=is_rest, is_grace=is_grace, is_chord=is_chord,
        is_unpitched=is_unpitched, duration_missing=duration_missing,
        tie_start="start" %in% ties, tie_stop="stop" %in% ties,
        midi=midi, step=step, octave=octave, alter=alter,
        pitch_class=pcint, pitch_class_12=pc12,
        pc_name=if (!is.na(pcint)) pc_name(pcint, flats=flats) else NA_character_,
        interval=NA_real_, contour=NA_character_, stringsAsFactors=FALSE
      )

      for (ln in xml2::xml_find_all(child, "lyric")) {
        verse <- .value_or(xml2::xml_attr(ln, "number"), "1")
        raw <- .lyric_text(ln)
        clean <- if (is.na(raw)) NA_character_ else trimws(raw)
        present <- !is.na(clean) && nzchar(clean)
        en <- xml2::xml_find_first(ln, "extend")
        has_extend <- !inherits(en, "xml_missing")
        extend_type <- if (has_extend) .value_or(xml2::xml_attr(en, "type"), "unspecified") else NA_character_
        ly_rows[[length(ly_rows) + 1L]] <- data.frame(
          event_id=event_id, part_id=part_id, voice=voice, staff=staff, verse=as.character(verse),
          lyric_name=.value_or(xml2::xml_attr(ln, "name"), NA_character_),
          syllabic=tolower(.value_or(.xml_text1(ln, "syllabic"), if (present) "single" else NA_character_)),
          text=if (present) clean else NA_character_, text_raw=raw, text_present=present,
          has_elision=.xml_has(ln, "elision"), has_extend=has_extend,
          extend_type=extend_type, stringsAsFactors=FALSE
        )
      }
    }

    nominal_q <- nominal_duration_q
    actual_q <- max_q
    advance_q <- if (implicit) {
      if (actual_q > 0) actual_q else if (is.finite(nominal_q)) nominal_q else 0
    } else if (is.finite(nominal_q)) {
      max(actual_q, nominal_q, na.rm = TRUE)
    } else if (actual_q > 0) actual_q else 0
    me_rows[[length(me_rows) + 1L]] <- data.frame(
      part_id=part_id, measure=if (is.na(numeric_label)) mi else numeric_label,
      measure_index=mi, measure_label=label, divisions=divisions, beats=beats,
      beat_type=beat_type, time_signature=time_signature,
      meter_supported=meter_supported, fifths=fifths, implicit=implicit,
      measure_onset_q=measure_start_q, actual_duration_q=actual_q,
      nominal_duration_q=nominal_q, stringsAsFactors=FALSE
    )
    measure_start_q <- measure_start_q + advance_q
  }

  list(events=.rbind_fill(ev_rows, .empty_events()),
       lyrics=.rbind_fill(ly_rows, .empty_lyrics()),
       measures=.rbind_fill(me_rows, .empty_measures()), n_events=event_n)
}
