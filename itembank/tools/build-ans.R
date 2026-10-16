# ---------------------------------------------------------------
# Build an ANS package from a folder of R/exams questions (.qmd or .Rmd).
# ---------------------------------------------------------------
# Usage, from the itembank folder (or give any path to the question folder):
#     Rscript tools/build-ans.R examples                         # preview + package
#     Rscript tools/build-ans.R examples --html                  # preview only, faster
#     Rscript tools/build-ans.R examples --out=/tmp/x            # write the output elsewhere
#     Rscript tools/build-ans.R examples --title-prefix="TEST "  # e.g. for a trial import
#     Rscript tools/build-ans.R examples --feedback              # practice tests: every choice
#                                                                #   question shows its solution
#                                                                #   as feedback
#     Rscript tools/build-ans.R examples --seed=2027             # redraw randomized questions
#
# Writes, inside the question folder (or --out):
#     <NAME>.zip            QTI 2.1 package: import in ANS as "QTI 2.X package (Legacy)"
#     <NAME>.html           preview of every question with its solution, for review
#     <NAME>.expected.json  what each item should look like after import, for
#                           tools/check-import.py
#
# Question order follows the sorted file names, so number the files (1-..., 2-...).
# The parts of a multi-part exercise must sort next to each other.
#
# exams2qti21() writes standard QTI 2.1, which ANS only half understands. ans_package.py
# (next to this script) reshapes the package into the dialect ANS imports cleanly. Each
# rule was settled by test imports at the University of Amsterdam (Sept-Oct 2026); see
# ans-agent-instructions.qmd for the evidence:
#   - the Solution becomes the model answer, and a "**Criteria**" list in an open
#     question's Solution becomes ANS's grading criteria;
#   - files sharing exextra[exercise] become one multi-part exercise;
#   - item titles come from exextra[anstitle], not "1", "2", ...;
#   - figures and data files travel inside the package, maths as ANS's own $$...$$.
#
# Needs R with the packages exams and jsonlite, pandoc (RStudio ships one), and Python 3.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L)
  stop("usage: Rscript tools/build-ans.R <question folder> [--html] [--out=dir] ",
       "[--title-prefix=TEXT] [--feedback] [--seed=N]")

htmlOnly    <- "--html" %in% args
feedbackAll <- "--feedback" %in% args
optValue <- function(flag, default = "") {
  hit <- grep(paste0("^--", flag, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", flag, "="), "", hit[1L]) else default
}
titlePrefix <- optValue("title-prefix")
seed        <- as.integer(optValue("seed", "1"))
target      <- args[!grepl("^--", args)][1L]

# This script's own location, so the build works from any working directory.
selfPath <- normalizePath(sub("^--file=", "",
                             grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]))
toolsDir <- dirname(selfPath)
bankRoot <- dirname(toolsDir)
folder <- if (dir.exists(target)) target else file.path(bankRoot, target)
if (!dir.exists(folder)) stop("no such folder: ", target)
folder <- normalizePath(folder)
outdir <- optValue("out", folder)
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
outdir <- normalizePath(outdir)

# NAME: the folder's name in capitals, e.g. examples -> EXAMPLES
name <- toupper(gsub("[^A-Za-z0-9]+", "_", basename(folder)))

files <- sort(list.files(folder, pattern = "\\.(qmd|Rmd)$", full.names = FALSE))
if (!length(files)) stop("no .qmd or .Rmd questions in ", folder)
cat("Building ", name, " from ", length(files), " questions:\n", sep = "")
cat(paste0("  ", files, collapse = "\n"), "\n\n")

owd <- setwd(folder); on.exit(setwd(owd))
library(exams)

# The preview is a review sheet: a little styling, and above every question its ANS
# title, exercise and part, type and points.
css <- c(
  "body { font-family: -apple-system, 'Segoe UI', Helvetica, Arial, sans-serif; max-width: 52rem;",
  "       margin: 2rem auto; padding: 0 1rem; line-height: 1.5; color: #222; }",
  "h2 { color: #BC0031; margin-bottom: 0.2rem; }",
  ".intro { color: #555; margin-top: 0; }",
  ".item { margin: 2.2rem 0 0.4rem; padding: 0.45rem 0.8rem; background: #f5f5f5;",
  "        border-left: 4px solid #BC0031; font-size: 0.95rem; }",
  ".item .title { font-weight: 600; }",
  "h4 { margin: 0.9rem 0 0.3rem; }",
  "table { border-collapse: collapse; } td, th { border: 1px solid #ccc; padding: 2px 8px; }",
  "pre { background: #f5f5f5; padding: 0.5rem 0.8rem; overflow-x: auto; }")
template <- file.path(tempdir(), "preview.html")
writeLines(c("<!DOCTYPE html>", "<html>", "<head>",
             paste0("<title>", name, " (preview)</title>"), "<meta charset=\"utf-8\" />",
             "<style type=\"text/css\">", css, "</style>", "</head>", "<body>",
             paste0("<h2>", name, "</h2>"),
             paste0("<p class=\"intro\">Preview of the package: every numbered item is one question ",
                    "in ANS, with its solution (the model answer) below it. In choice questions the ",
                    "True/False list marks the key, in the option order the package fixes.</p>"),
             "##\\exinput{exercises}##", "</body>", "</html>"), template)

set.seed(seed)
shown <- exams2html(as.list(files), n = 1, name = name, template = template,
                    converter = "pandoc-mathjax", dir = outdir,
                    question = "<h4>Question</h4>", solution = "<h4>Solution</h4>")
preview <- file.path(outdir, paste0(name, ".html"))
invisible(file.rename(file.path(outdir, paste0(name, "1.html")), preview))  # drop the replication number

kind <- function(mi) {
  if (isTRUE(mi$essay)) return("open question")
  switch(mi$type, schoice = "single choice", mchoice = "tick-all (multiple response)",
         num = "one numeric gap", cloze = paste(length(mi$solution), "gaps"),
         string = "short text answer", mi$type)
}
mis <- lapply(shown[[1L]], function(e) e$metainfo)
ex  <- vapply(mis, function(mi) if (is.null(mi$exercise)) mi$file else mi$exercise, character(1L))
grp <- cumsum(c(TRUE, ex[-1L] != ex[-length(ex)]))           # consecutive parts form one exercise
heads <- vapply(seq_along(mis), function(k) {
  mi <- mis[[k]]
  n  <- sum(grp == grp[k]); j <- sum(grp[seq_len(k)] == grp[k])
  pts <- if (is.null(mi$points)) 1 else sum(unlist(mi$points))
  title <- paste0(titlePrefix, if (is.null(mi$anstitle)) mi$file else mi$anstitle)
  where <- if (n > 1) sprintf("Exercise %d, part %d of %d", grp[k], j, n) else sprintf("Exercise %d", grp[k])
  sprintf("<div class=\"item\"><span class=\"title\">%s</span><br/>%s &middot; %s &middot; %s point%s &middot; %s</div>\n",
          title, where, kind(mi), format(round(pts, 3)), if (pts == 1) "" else "s", mi$file)
}, character(1L))
txt <- paste(readLines(preview, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
chunks <- strsplit(txt, "<h4>Question</h4>", fixed = TRUE)[[1L]]
if (length(chunks) == length(heads) + 1L) {
  txt <- paste0(chunks[1L], paste0(heads, "<h4>Question</h4>", chunks[-1L], collapse = ""))
  writeLines(txt, preview, useBytes = TRUE)
}
cat("wrote ", name, ".html (preview)\n", sep = "")

if (htmlOnly) quit(save = "no")

# Same seed as the preview, so randomized questions show the same numbers in both.
set.seed(seed)
raw <- file.path(tempdir(), paste0(name, "_raw"))
unlink(raw, recursive = TRUE); dir.create(raw)
built <- exams2qti21(as.list(files), n = 1, name = name, dir = raw,
                     converter = "pandoc-mathjax", base64 = FALSE,
                     schoice = list(enumerate = FALSE))

# What ans_package.py needs from each question's metadata. R/exams flattens
# exextra[...] into the metainfo, so exextra[anstitle] arrives as $anstitle.
items <- unname(lapply(built[[1L]], function(e) {
  mi <- e$metainfo
  list(rmd      = mi$file,
       type     = mi$type,
       anstitle = if (is.null(mi$anstitle)) mi$file else mi$anstitle,
       exercise = mi$exercise,
       alt      = mi$alt,      # exextra[alt]: extra accepted answers per gap ("a b|c")
       # a cloze lists points per gap (R/exams recycles expoints per gap): sum them
       points   = if (is.null(mi$points)) 1 else round(sum(unlist(mi$points)), 3))
}))
meta <- file.path(raw, "items.json")
jsonlite::write_json(list(name = name, title_prefix = titlePrefix,
                          feedback_all = feedbackAll, items = items),
                     meta, auto_unbox = TRUE, null = "null", pretty = TRUE)

zipOut <- file.path(outdir, paste0(name, ".zip"))
status <- system2("python3", c(shQuote(file.path(toolsDir, "ans_package.py")),
                               shQuote(file.path(raw, paste0(name, ".zip"))),
                               shQuote(meta), shQuote(zipOut),
                               "--expected", shQuote(file.path(outdir, paste0(name, ".expected.json")))))
if (status != 0L) stop("reshaping for ANS failed, see the message above")

titles <- vapply(items, function(i) paste0(titlePrefix, i$anstitle), character(1L))
cat("\nItem titles as ANS will show them (parts of one exercise share a title):\n")
cat(paste0("  ", seq_along(titles), ". ", titles, collapse = "\n"), "\n")
