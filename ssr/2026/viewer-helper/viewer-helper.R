# Shared code for the per-lecture viewer-helper pages (<lecture folder>/viewer-helper.qmd).
# Each page sources this file with its working directory in the lecture folder.
#
# chapters.csv has one row per chapter: lecture number, start (h:mm:ss), title and a one-
# or two-sentence summary lifted from the transcript (fillers and mistranscriptions
# cleaned, no student or colleague names). The start is the paragraph in
# transcripts/condensed/Lnn.txt where the topic begins; on L21 that matched the
# recording to within seconds (his check, 2026-11-12). The recording is the lecture
# wrapper's view: key, the same link the overview's Watch column shows. Times are plain
# text: Stream deep links (playbackOptions.startTimeInSeconds) opened a white page after
# the UvA login (2026-11-12).
chapters <- read.csv("../viewer-helper/chapters.csv", check.names = FALSE, encoding = "UTF-8")

lectureInfo <- function(lecture) {
  file <- list.files(".", pattern = paste0("^", lecture, "\\..*\\.qmd$"))
  yaml <- rmarkdown::yaml_front_matter(file)
  list(date = yaml$date, view = yaml$view,
       handout = URLencode(sub("\\.qmd$", ".html", file)),
       slides  = URLencode(sub("\\.qmd$", ".slide.html", file)))
}

toSeconds <- function(hms) {
  vapply(strsplit(hms, ":"), function(x) sum(as.numeric(x) * c(3600, 60, 1)), numeric(1))
}
timeLabel <- function(s) {
  ifelse(s >= 3600, sprintf("%d:%02d:%02d", s %/% 3600, s %% 3600 %/% 60, s %% 60),
         sprintf("%d:%02d", s %/% 60, s %% 60))
}

# Date and links at the top of the page.
lectureLine <- function(lecture) {
  info  <- lectureInfo(lecture)
  links <- c(if (!is.null(info$view)) sprintf("[Recording](%s){target=\"_blank\"}", info$view),
             sprintf("[Slides](%s){target=\"_blank\"}", info$slides),
             sprintf("[Handout](%s){target=\"_blank\"}", info$handout))
  paste(c(info$date, links), collapse = " · ")
}

chapterTable <- function(lecture) {
  rows    <- chapters[chapters$lecture == lecture, ]
  isBreak <- rows$chapter == "Break"
  time    <- timeLabel(toSeconds(rows$start))
  summary <- ifelse(rows$summary == "", "",
                    sprintf('<span class="chapter-summary">%s</span>', htmltools::htmlEscape(rows$summary)))
  title   <- ifelse(isBreak, "<em>Break</em>", paste0(htmltools::htmlEscape(rows$chapter), summary))
  knitr::kable(data.frame(Time = time, Chapter = title), format = "html", escape = FALSE,
               row.names = FALSE, table.attr = 'class="table table-sm chapters"')
}
