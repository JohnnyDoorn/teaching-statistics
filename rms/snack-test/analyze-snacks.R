# Snack demo, RMS lecture 18: is nationality associated with how you feel about Dutch snacks?
#
# In class: students fill in the form (edu.nl/wvcrk, "RMS Snack Assessment 2026"), you download
# the responses from Forms (Responses > Open results in Excel, or a .csv), source this script and
# refresh the results page (html_file) in the browser. Without a path it reads the newest download
# named after the form, "RMS Snack Assessment 2026(1-64).xlsx" (the range in brackets grows with the
# responses), so an older year's export in ~/Downloads is never used.
#   RStudio:   source("analyze-snacks.R")
#   Terminal:  Rscript analyze-snacks.R "~/Downloads/RMS Snack Assessment 2026(1-64).xlsx"
#
# Per snack: observed and expected counts, the conditional proportions, the chi-squared test and
# one figure (the bars of P(opinion | nationality) as on the electroshock slide, and the chi-squared
# distribution with the critical value and the observed X^2, as in the p-value slides).
# No continuity correction, as on the slides (z^2 = X^2) and in JASP's default output; on the 2025
# data this script gives JASP's numbers (X^2 = 3.585, 68.534, 22.659).
#
# The results page is one self-contained file in the repo; slide 78 of L18 links to it. Commit and
# push it after class: a trial run writes the same file, so don't commit before.

data_file <- NULL     # NULL: newest download whose name starts with form_name (or give a path)
form_name <- "RMS Snack Assessment 2026"   # Forms names the export "<form name>(1-N).xlsx"
alpha     <- 0.01     # set before the data come in (2025 used 0.01 for all three tests)
drop_ids  <- c(1)     # Ids of responses to leave out: 1 = the trial of 12 Nov 09:43
html_file <- "~/GitHubStuff/teaching-statistics/datasets/2026/snack-test-2026.html"   # the page to refresh

love_hate_col <- c(Love = "green", Hate = "red")   # good outcome green, as Recovery on the slides


# --- read the export ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (is.null(data_file) && length(args) > 0) data_file <- args[1]
if (is.null(data_file)) {
  files <- list.files("~/Downloads", pattern = "\\.(xlsx|csv)$", ignore.case = TRUE, full.names = TRUE)
  files <- files[startsWith(basename(files), form_name)]   # also skips Excel's ~$ lock file
  if (length(files) == 0)
    stop("No download named '", form_name, "(...).xlsx' in ~/Downloads: download the responses, or set data_file at the top.")
  data_file <- files[which.max(file.mtime(files))]
}
data_file <- path.expand(data_file)

if (grepl("\\.xlsx$", data_file, ignore.case = TRUE)) {
  dat <- as.data.frame(readxl::read_excel(data_file, .name_repair = "minimal"))
} else {
  # Excel in a Dutch locale writes ; as the separator
  first <- readLines(data_file, n = 1, warn = FALSE)
  sep <- if (lengths(gregexpr(";", first)) > lengths(gregexpr(",", first))) ";" else ","
  dat <- read.csv(data_file, sep = sep, check.names = FALSE, stringsAsFactors = FALSE,
                  na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
}

# Forms adds Id / time / name columns (in Dutch or English); the questions are found by their text
nat_col    <- grep("nationality", names(dat), ignore.case = TRUE, value = TRUE)[1]
snack_cols <- grep("opinion on", names(dat), ignore.case = TRUE, value = TRUE)
if (is.na(nat_col) || length(snack_cols) == 0)
  stop("Cannot find the questions. Columns in the file: ", paste(names(dat), collapse = " | "))
snacks <- sub(".*opinion on\\s*(.*?)\\s*\\?*$", "\\1", snack_cols, ignore.case = TRUE, perl = TRUE)

id_col <- grep("^id$", names(dat), ignore.case = TRUE, value = TRUE)[1]
if (length(drop_ids) > 0 && !is.na(id_col)) dat <- dat[!(dat[[id_col]] %in% drop_ids), ]
if (nrow(dat) == 0) stop("No responses left in ", basename(data_file), " (after drop_ids).")

clean <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  x
}
in_order <- function(x, first) {   # the form's answer order first, anything unexpected after it
  seen <- unique(x[!is.na(x)])
  c(intersect(first, seen), sort(setdiff(seen, first)))
}
nat <- clean(dat[[nat_col]])

# first and last start time in file order, so a test response from before class stands out
time_col <- grep("^(start time|begintijd)$", names(dat), ignore.case = TRUE, value = TRUE)[1]
times <- if (is.na(time_col)) character(0) else format(dat[[time_col]])
times <- times[!is.na(times) & times != "" & times != "NA"]
cat(sprintf("\nFile: %s\nResponses: %d", basename(data_file), nrow(dat)))
if (length(times) > 0) cat(sprintf(" (first %s, last %s)", times[1], times[length(times)]))
cat(sprintf("\nalpha = %s\n", alpha))


# --- one test per snack ------------------------------------------------------------------------

fmt_p <- function(p) if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)

analyze <- function(snack, col) {
  op  <- clean(dat[[col]])
  ok  <- !is.na(nat) & !is.na(op)
  tab <- table(Nationality = factor(nat[ok], levels = in_order(nat[ok], c("Dutch", "Non-Dutch"))),
               Opinion     = factor(op[ok],  levels = in_order(op[ok],  c("Love", "Hate"))))
  res <- list(snack = snack, tab = tab, cp = prop.table(tab, 1), n = sum(tab), test = NULL)

  cat(sprintf("\n==== %s (N = %d) ====\n\nObserved counts\n", snack, res$n))
  print(addmargins(tab))
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    cat("\nEveryone in one category: no association to test.\n")
    return(res)
  }
  res$test <- suppressWarnings(chisq.test(tab, correct = FALSE))
  cat("\nExpected counts under H0 (no association)\n")
  print(round(addmargins(res$test$expected), 3))

  cat("\nConditional proportions\n")
  for (g in rownames(tab)) cat(sprintf("  P(%s | %s) = %d/%d = %.3f\n", colnames(tab)[1], g,
                                       tab[g, 1], sum(tab[g, ]), res$cp[g, 1]))
  if (all(dim(tab) == 2)) {
    cat(sprintf("  Difference = %.3f, relative risk = %.3f / %.3f = %.2f\n", res$cp[1, 1] - res$cp[2, 1],
                res$cp[1, 1], res$cp[2, 1], res$cp[1, 1] / res$cp[2, 1]))
  }

  x2 <- unname(res$test$statistic); df <- unname(res$test$parameter); p <- res$test$p.value
  cat(sprintf("\nX^2 = %.3f, df = %d, %s -> %s\n", x2, df, fmt_p(p),
              if (p < alpha) "p < alpha: reject H0" else "p > alpha: fail to reject H0"))
  if (min(res$test$expected) < 5)
    cat(sprintf("Note: smallest expected count is %.2f (below 5), so the chi-squared approximation is shaky.\n",
                min(res$test$expected)))
  res
}


# --- figures -----------------------------------------------------------------------------------

draw <- function(res) {
  tab <- res$tab; cp <- res$cp
  old <- par(mfrow = c(1, nrow(tab) + 1), cex = 1.2, oma = c(0, 0, 4, 0), mar = c(4.5, 4.5, 3, 1))
  on.exit(par(old))

  cols <- ifelse(colnames(cp) %in% names(love_hate_col), love_hate_col[colnames(cp)], "grey")
  for (g in rownames(cp)) {
    b <- barplot(cp[g, ], names.arg = colnames(cp), col = cols, ylim = c(0, 1.05), las = 1,
                 ylab = if (g == rownames(cp)[1]) "Conditional proportion" else "",
                 main = sprintf("%s (n = %d)", g, sum(tab[g, ])))
    text(b, cp[g, ] + 0.05, sprintf("%.2f", cp[g, ]), xpd = NA)   # xpd: a 1.00 label sits above the plot
  }

  mtext(res$snack, outer = TRUE, line = 2, cex = 1.8)
  if (is.null(res$test)) {
    plot.new(); text(0.5, 0.5, "Everyone in one category:\nno test")
    return(invisible())
  }

  x2 <- unname(res$test$statistic); df <- unname(res$test$parameter); p <- res$test$p.value
  crit <- qchisq(1 - alpha, df)
  xmax <- max(15, 1.15 * x2, 1.3 * crit)
  ymax <- if (df == 1) 1 else 1.25 * max(dchisq(seq(0.05, xmax, length.out = 500), df))
  curve(dchisq(x, df), from = 0, to = xmax, n = 1000, ylim = c(0, ymax), bty = "n", lwd = 2, col = "blue",
        las = 1, xlab = bquote(chi^2 ~ "statistic"), ylab = "Density",
        main = bquote(chi^2 ~ "Distribution (df =" ~ .(df) * ")"))
  sq <- seq(0, crit, length.out = 1e3) + 0.001
  polygon(c(0, sq, crit, rev(sq)), c(0, dchisq(sq, df), 0, rep(0, 1e3)), col = "lightgreen", border = "blue")
  if (crit > 0.2 * xmax)   # no room for the label when a large X^2 stretches the axis
    text(max(crit / 3, 0.08 * xmax), 0.07 * ymax, paste0(100 * (1 - alpha), "%"), cex = 1.2)
  arrows(crit, -0.2, crit, 0.31 * ymax, lwd = 4)
  text(crit, 0.45 * ymax, round(crit, 2), cex = 1.1)
  arrows(x2, -0.2, x2, 0.5 * ymax, lwd = 4, col = "purple")
  text(x2, 0.62 * ymax, sprintf("%.2f", x2), cex = 1.1, col = "purple")

  verdict <- if (p < alpha) quote("reject" ~ H[0]) else quote("fail to reject" ~ H[0])
  mtext(bquote(X^2 == .(sprintf("%.2f", x2)) * "," ~ "df =" ~ .(df) * "," ~ .(fmt_p(p)) * ";" ~
                 alpha == .(alpha) * "," ~ .(verdict)), outer = TRUE, line = 0.3, cex = 1.3)
}


# --- results page for the students (one self-contained file, figures inlined) ------------------

esc <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
X2  <- "<i>X</i><sup>2</sup>"

html_table <- function(tab, digits = 0) {
  m <- addmargins(tab)
  dimnames(m) <- lapply(dimnames(m), function(x) replace(x, x == "Sum", "Total"))   # "Total", as the slides
  cells <- function(v) paste0("<td>", formatC(v, format = "f", digits = digits), "</td>", collapse = "")
  paste0("<table><tr><th></th>", paste0("<th>", esc(colnames(m)), "</th>", collapse = ""), "</tr>",
         paste0("<tr><th>", esc(rownames(m)), "</th>", apply(m, 1, cells), "</tr>", collapse = ""), "</table>")
}

figure_uri <- function(res) {
  f <- tempfile(fileext = ".png")
  png(f, width = 13, height = 5.5, units = "in", res = 110)
  draw(res)
  invisible(dev.off())
  paste0("data:image/png;base64,", base64enc::base64encode(f))
}

snack_html <- function(res) {
  tab <- res$tab; cp <- res$cp; t <- res$test
  out <- c(sprintf("<h2>%s <small>(N = %d)</small></h2>", esc(res$snack), res$n),
           "<div class=\"tables\"><div><h3>Observed counts</h3>", html_table(tab), "</div>")
  img <- sprintf("<img src=\"%s\" alt=\"Conditional proportions and chi-squared test for %s\">",
                 figure_uri(res), esc(res$snack))
  if (is.null(t)) return(c(out, "</div><p>Everyone answered in one category, so there is no association to test.</p>", img))

  x2 <- unname(t$statistic); df <- unname(t$parameter); p <- t$p.value
  props <- sprintf("P(%s | %s) = %d/%d = %.3f", esc(colnames(tab)[1]), esc(rownames(tab)), tab[, 1], rowSums(tab), cp[, 1])
  if (all(dim(tab) == 2))
    props <- c(props, sprintf("Difference = %.3f, relative risk = %.3f / %.3f = %.2f",
                              cp[1, 1] - cp[2, 1], cp[1, 1], cp[2, 1], cp[1, 1] / cp[2, 1]))
  verdict <- if (p < alpha) {
    sprintf("Because p &lt; %s, we reject H<sub>0</sub>. There is evidence for an association between nationality and the opinion on %s.",
            alpha, esc(res$snack))
  } else {
    sprintf("Because p &gt; %s, we fail to reject H<sub>0</sub>. There is no evidence for an association between nationality and the opinion on %s (absence of evidence).",
            alpha, esc(res$snack))
  }
  c(out, "<div><h3>Expected counts if H<sub>0</sub> is true</h3>", html_table(t$expected, 3), "</div></div>",
    "<h3>Conditional proportions</h3><ul>", paste0("<li>", props, "</li>"), "</ul>",
    sprintf("<p class=\"test\">%s = %.3f, df = %d, %s</p>", X2, x2, df, esc(fmt_p(p))),
    sprintf("<p>%s</p>", verdict),
    if (min(t$expected) < 5)
      sprintf("<p class=\"note\">The smallest expected count is %.2f, below 5, so the large-sample assumption is not met.</p>",
              min(t$expected)),
    img)
}

page_css <- "
:root { color-scheme: light; }
body { font-family: 'PT Sans', -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; line-height: 1.5;
       max-width: 980px; margin: 0 auto; padding: 16px; color: #1a1a1a; background: #fff; }
h1 { margin-bottom: 0; }
h2 { margin-top: 2em; border-bottom: 1px solid #ccc; padding-bottom: 4px; }
h2 small { font-weight: normal; color: #666; }
h3 { margin-bottom: 4px; }
.sub { color: #666; margin-top: 4px; }
table { border-collapse: collapse; margin: 8px 0 12px; }
th, td { border: 1px solid #ccc; padding: 4px 10px; text-align: right; }
th { background: #f3f3f3; }
tr th:first-child, table.summary td:first-child, table.summary td:last-child { text-align: left; }
.tables { display: flex; flex-wrap: wrap; column-gap: 40px; }
.box { background: #f2f6fa; border: 2px solid #4a7ab5; padding: 0 16px; margin: 24px 0; }
.test { font-size: 1.15em; margin-bottom: 0; }
.note { color: #b45309; }
img { width: 100%; height: auto; }
footer { margin-top: 40px; color: #777; font-size: 0.85em; }
"

write_html <- function(file) {
  file <- path.expand(file)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  day  <- Sys.Date()
  year <- format(day, "%Y")
  when <- sprintf("%d %s %s", as.integer(format(day, "%d")), month.name[as.integer(format(day, "%m"))], year)
  rows <- apply(summary_tab, 1, function(r) paste0("<td>", sub("H0", "H<sub>0</sub>", esc(trimws(r))), "</td>", collapse = ""))
  page <- c(
    "<!doctype html>", "<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    sprintf("<title>Snack test %s</title>", year), "<style>", page_css, "</style></head><body>",
    sprintf("<h1>Snack test %s</h1>", year),
    sprintf("<p class=\"sub\">Research Methods and Statistics, demonstration in class on %s</p>", when),
    sprintf(paste("<p>Is nationality associated with how you feel about Dutch snacks? %d of you answered. For each snack,",
                  "we test H<sub>0</sub> (nationality and opinion are independent) against H<sub>A</sub> (they are dependent),",
                  "with &alpha; = %s, chosen before the data came in. Each figure shows the conditional proportions per",
                  "nationality, and the %s distribution with the critical value (black arrow) and our %s (purple arrow).</p>"),
            nrow(dat), alpha, X2, X2),
    sprintf("<table class=\"summary\"><tr><th>Snack</th><th>N</th><th>%s</th><th>df</th><th>p</th><th>Decision</th></tr>", X2),
    paste0("<tr>", rows, "</tr>"), "</table>",
    "<div class=\"box\"><h3>Why only the right tail?</h3>",
    sprintf(paste("<p>The %s test is always two-sided. Differences between the observed and the expected counts, in either",
                  "direction, make %s larger, which is why only one tail (the right one) is extreme. Based on a significant",
                  "%s, we can therefore only make a two-sided conclusion (there is an association, not a &ldquo;positive&rdquo;",
                  "or &ldquo;negative&rdquo; one). The conditional proportions then describe how the groups differ in our sample.</p></div>"),
            X2, X2, X2),
    unlist(lapply(results, snack_html)),
    "<footer>Computed in R with chisq.test() without continuity correction, which gives the same numbers as JASP's Contingency Tables.</footer>",
    "</body></html>")
  con <- file(file, open = "w", encoding = "UTF-8")
  writeLines(page, con)
  close(con)
  file
}


# --- run ---------------------------------------------------------------------------------------

results <- Map(analyze, snacks, snack_cols)

summary_tab <- do.call(rbind, lapply(results, function(res) {
  if (is.null(res$test)) return(data.frame(Snack = res$snack, N = res$n, X2 = "-", df = "-", p = "-", Decision = "no test"))
  p <- res$test$p.value
  data.frame(Snack = res$snack, N = res$n, X2 = sprintf("%.3f", unname(res$test$statistic)),
             df = as.character(unname(res$test$parameter)), p = if (p < 0.001) "< 0.001" else sprintf("%.3f", p),
             Decision = if (res$test$p.value < alpha) "reject H0" else "fail to reject H0")
}))
cat(sprintf("\nSummary (alpha = %s)\n", alpha))
print(summary_tab, row.names = FALSE)

cat(sprintf("\nResults page written, refresh it in the browser:\n%s\n", write_html(html_file)))
