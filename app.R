## STA304 – Students & Generative AI: interactive results
## Keep this file and survey_data.csv in the same folder.

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(DT)

# ------------------------------------------------------------------ DATA ----
raw <- read_csv("survey_data.csv", show_col_types = FALSE)

cgpa_levels <- c("Below 2.7", "2.7\u20132.99", "3.0\u20133.29", "3.3\u20133.69", "3.7\u20134.0")
cgpa_mids   <- c(1.35, 2.845, 3.145, 3.495, 3.85)

survey <- raw |>
  mutate(
    status    = factor(if_else(international == 1, "International", "Domestic"),
                       levels = c("Domestic", "International")),
    cgpa_band = factor(cgpa_levels[match(round(cgpa_midpoint, 3), cgpa_mids)],
                       levels = cgpa_levels)
  )

# Readable names (nobody should see code-book names)
num_labels <- c(
  "Weekly study hours"                = "study_hours_week",
  "Coursework difficulty (1\u201310)" = "coursework_difficulty",
  "AI use frequency (1\u201310)"      = "ai_use_frequency",
  "Confidence without AI (1\u201310)" = "confidence_without_ai",
  "Hours per week AI saves"           = "ai_hours_saved_week",
  "Estimated cGPA (band midpoint)"    = "cgpa_midpoint"
)
pred_labels <- num_labels[num_labels != "confidence_without_ai"]
mean_labels <- num_labels[match(c("ai_hours_saved_week", "ai_use_frequency", "confidence_without_ai",
                                          "coursework_difficulty"), num_labels)]
label_of <- function(v) names(num_labels)[match(v, num_labels)]
short_of <- function(v) {
  s <- sub(" \\(.*\\)$", "", label_of(v))
  if (startsWith(s, "AI")) s else paste0(tolower(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

likert_blocks <- list(
  tasks = list(
    title = "Academic uses of AI",
    question = "Which academic uses of AI are most common?",
    vars = c("Writing / editing" = "task_writing_editing", "Double-checking work" = "task_double_checking",
             "Summarizing" = "task_summarizing", "Brainstorming" = "task_brainstorming",
             "Problem solving" = "task_problem_solving", "Other uses" = "task_other"),
    levels = c("Never", "Rarely", "Sometimes", "Often", "Very Often"),
    topword = "use it at least \u201cSometimes\u201d", atleast = "at least \u201cSometimes\u201d", legend = "How often"
  ),
  replace = list(
    title = "Learning activities replaced by AI",
    question = "Does AI replacement differ across traditional learning activities?",
    vars = c("Course readings" = "replace_readings", "Reviewing lecture materials" = "replace_lecture_review",
             "Attending lectures / tutorials" = "replace_attendance", "Help from instructors / TAs" = "replace_instructor_help",
             "Help from classmates" = "replace_peer_help", "Searching the internet" = "replace_internet_search",
             "Other activities" = "replace_other"),
    levels = c("Never", "Rarely", "Sometimes", "Often", "Very Often"),
    topword = "use AI instead of this at least \u201cSometimes\u201d", atleast = "at least \u201cSometimes\u201d", legend = "How often"
  ),
  importance = list(
    title = "Reasons for using AI",
    question = "Which reasons for using AI matter most to students?",
    vars = c("Saving time" = "importance_save_time", "Immediate help" = "importance_immediate_help",
             "Understanding course material" = "importance_understanding",
             "Limited access to other support" = "importance_limited_support",
             "Improving quality of work" = "importance_work_quality", "Other reasons" = "importance_other"),
    levels = c("Not Important", "Slightly Important", "Moderately Important", "Very Important", "Extremely Important"),
    topword = "rate it at least \u201cModerately Important\u201d", atleast = "at least \u201cModerately Important\u201d", legend = "Importance"
  )
)
block_choices <- setNames(names(likert_blocks), vapply(likert_blocks, `[[`, "", "title"))

display_cols <- c(
  "Student status" = "status", "cGPA band" = "cgpa_band", num_labels,
  setNames(unlist(lapply(likert_blocks, \(b) b$vars)),
           unlist(lapply(likert_blocks, \(b) paste0(b$title, ": ", names(b$vars)))))
)

palettes <- list(
  "Blue & green (default)"         = c("#2563A8", "#2A9D8F", "#E9A23B", "#7A8CA3", "#D1495B", "#8C8C9E"),
  "Colour-blind safe (Okabe\u2013Ito)" = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00", "#56B4E9", "#F0E442"),
  "Sunset"                         = c("#E4572E", "#29335C", "#F3A712", "#669BBC", "#A8C686"),
  "Greyscale"                      = c("#25243A", "#9A9AA8", "#5B5B6E", "#C9C9D2", "#3A3A4A")
)

# --------------------------------------------------------- TEXT HELPERS ----
fmt_p <- function(p) if (p < 0.001) "p < 0.001" else paste0("p = ", formatC(p, format = "f", digits = 3))

strength_word <- function(r) {
  a <- abs(r)
  if (a < 0.1) "almost no" else if (a < 0.3) "a weak" else if (a < 0.5) "a moderate" else "a strong"
}

interp_box <- function(...) div(class = "interp", strong("Interpretation: "), ...)
# shown next to every result that uses cGPA
cgpa_note <- " Estimated cGPA uses the middle of each cGPA band, and \u201cBelow 2.7\u201d is placed at 1.35 (the middle of 0\u20132.7), so results involving cGPA depend on that choice."

cronbach <- function(x) {
  x <- as.matrix(x[complete.cases(x), , drop = FALSE])
  if (nrow(x) < 5 || ncol(x) < 2) return(NA_real_)
  v <- var(rowSums(x)); if (is.na(v) || v == 0) return(NA_real_)
  k <- ncol(x); k / (k - 1) * (1 - sum(apply(x, 2, var)) / v)
}
alpha_label <- function(a) as.character(cut(a, c(-Inf, .5, .6, .7, .8, .9, Inf), right = FALSE,
  labels = c("unacceptable", "poor", "questionable", "acceptable", "good", "excellent")))
# named with the same titles as likert_blocks, so each question set has one name everywhere
alpha_groups <- setNames(
  lapply(likert_blocks[c("replace", "tasks", "importance")], \(b) b$vars[!grepl("^Other", names(b$vars))]),
  vapply(likert_blocks[c("replace", "tasks", "importance")], `[[`, "", "title")
)
prop_row <- function(x) {
  x <- x[!is.na(x)]; n <- length(x); k <- sum(x >= 3)
  t <- prop.test(k, n, p = 0.5, correct = FALSE)
  c(n = n, p = k / n, lo = t$conf.int[1], hi = t$conf.int[2], pval = t$p.value)
}
two_prop_p <- function(kD, nD, kI, nI) {
  if (nD < 1 || nI < 1) return(c(NA, NA))
  m <- matrix(c(kD, nD - kD, kI, nI - kI), 2, byrow = TRUE)
  if (all(m >= 5)) c(suppressWarnings(prop.test(c(kD, kI), c(nD, nI))$p.value), 0)
  else c(fisher.test(m)$p.value, 1)
}
pct <- function(x) sprintf("%.0f%%", 100 * x)

# -------------------------------------------------------------------- UI ----
# ------------------------------------------------------------- CONFIG ----
# Everything look-and-feel lives here, so the app can be re-skinned in one place.
cfg <- list(
  title       = "Students & Generative AI",
  base_size   = 14,
  plot_height = 420,
  colours     = c(primary = "#2563A8", secondary = "#2A9D8F", ink = "#1F2A37",
                  muted = "#6B7785", bg = "#F6F9FB", line = "#E2E8EE")
)
fill_tpl <- function(tpl, vals) {
  for (k in names(vals)) tpl <- gsub(paste0("@", k, "@"), vals[[k]], tpl, fixed = TRUE)
  tpl
}

app_theme <- bs_theme(
  version = 5, primary = cfg$colours[["primary"]], secondary = cfg$colours[["secondary"]],
  bg = "#FFFFFF", fg = cfg$colours[["ink"]],
  base_font = font_collection("Segoe UI", "system-ui", "-apple-system", "Helvetica Neue", "sans-serif")
) |> bs_add_rules(fill_tpl("
  body { background: @bg@; line-height: 1.55; }
  .navbar { background: #fff !important; border-bottom: 1px solid @line@; box-shadow: none; }
  .navbar .navbar-brand { color: @primary@ !important; font-weight: 700; }
  .navbar .nav-link { color: @muted@ !important; font-weight: 500; padding: .95rem 1rem; }
  .navbar .nav-link:hover { color: @primary@ !important; }
  .navbar .nav-link.active { color: @primary@ !important; box-shadow: inset 0 -3px 0 @secondary@; }
  .page { max-width: 1080px; margin: 0 auto; padding: 1.5rem 1rem 3rem; }
  .card { border: 1px solid @line@; border-radius: .9rem; box-shadow: none; margin-bottom: 1.25rem; }
  .card-header { background: transparent; border-bottom: 1px solid @line@; font-weight: 600; padding: 1rem 1.25rem; }
  .card-body { padding: 1.25rem; }
  .ctrl-row { margin-bottom: .5rem; }
  .ctrl-row label, .sidebar label.control-label { font-size: .75rem; color: @muted@; font-weight: 600;
                                                   text-transform: uppercase; letter-spacing: .04em; }
  .interp { border-left: 4px solid @secondary@; background: @bg@; padding: .9rem 1.1rem;
            border-radius: .5rem; margin-top: 1rem; }
  .small-note { color: @muted@; font-size: .85rem; }
  .counter { font-weight: 600; color: @primary@; margin-bottom: .75rem; }
  .accordion-button { font-size: .9rem; font-weight: 600; }
  .accordion-button:not(.collapsed) { background: @bg@; color: @primary@; box-shadow: none; }
  .table { font-size: .9rem; }
", cfg$colours))

# ------------------------------------------------- REUSABLE UI BUILDERS ----
page_body  <- function(...) div(class = "page", ...)
ctrl_row   <- function(..., widths = NULL) layout_columns(fill = FALSE, col_widths = widths, class = "ctrl-row", ...)
# controls -> plot -> plain-English box -> collapsed numbers
analysis_body <- function(plot_id, interp_id = NULL, controls = NULL, details = NULL, height = cfg$plot_height) {
  tagList(
    controls,
    plotOutput(plot_id, height = height),
    if (!is.null(interp_id)) uiOutput(interp_id),
    if (!is.null(details)) accordion(open = FALSE, accordion_panel("Show the numbers", details))
  )
}
analysis_card <- function(question, ...) card(full_screen = TRUE, card_header(question), card_body(analysis_body(...)))

# ---------------------------------------------- REUSABLE PLOT FUNCTIONS ----
# Side-by-side distributions of one or more numeric measures, split by a grouping column.
# lookup: named vector label = column id (e.g. num_labels). fills: vector of group colours.
group_box_plot <- function(d, vars, lookup, group = "status", fills, type = c("box", "violin"),
                           points = TRUE, ylim = NULL) {
  type <- match.arg(type)
  long <- d |>
    select(all_of(c(group, vars))) |>
    pivot_longer(-all_of(group), names_to = "measure", values_to = "val") |>
    filter(!is.na(val)) |>
    mutate(measure = factor(names(lookup)[match(measure, lookup)],
                            levels = names(lookup)[match(vars, lookup)]))
  p <- ggplot(long, aes(.data[[group]], val, fill = .data[[group]]))
  p <- if (type == "violin")
    p + geom_violin(alpha = .55, colour = NA) +
      geom_boxplot(width = .14, fill = "white", linewidth = .5, outlier.shape = NA)
  else
    p + geom_boxplot(width = .55, alpha = .7, linewidth = .5, outlier.shape = if (points) NA else 1)
  # spread points sideways only (height = 0), so every point stays at the value the student reported
  if (points) p <- p + geom_jitter(width = .12, height = 0, alpha = .45, size = 1.6, colour = "#1F2A37")
  p <- p +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3.4, fill = "white", colour = "#1F2A37") +
    scale_fill_manual(values = rep_len(fills, nlevels(long[[group]])), guide = "none") +
    labs(x = "Student status", y = "Reported value")
  if (length(vars) > 1) p <- p + facet_wrap(~measure, scales = "free_y", nrow = 2)
  else p <- p + labs(y = levels(long$measure)) + coord_cartesian(ylim = ylim)
  p
}

# Two-group comparison of means for one measure; chooses pooled vs Welch via Bartlett's test.
compare_means <- function(d, v, group = "status") {
  x <- d[!is.na(d[[v]]), ]; g <- droplevels(x[[group]])
  bart <- tryCatch(bartlett.test(x[[v]] ~ g)$p.value, error = function(e) NA_real_)
  eq <- !is.na(bart) && bart >= 0.05
  t <- t.test(x[[v]] ~ g, var.equal = eq)
  data.frame(id = v, m1 = unname(t$estimate[1]), m2 = unname(t$estimate[2]), diff = unname(t$estimate[1] - t$estimate[2]),
             lo = t$conf.int[1], hi = t$conf.int[2], p = t$p.value, test = if (eq) "Pooled t-test" else "Welch\u2019s t-test")
}

# Stacked 100% / count bars for a Likert block. long: item, score (factor), status; item_order: items low -> high.
likert_bar <- function(long, block, item_order, fills, show = c("fill", "stack"), split = FALSE) {
  show <- match.arg(show)
  long$item <- factor(long$item, levels = item_order)
  p <- ggplot(long, aes(y = item, fill = score)) +
    geom_bar(position = show, colour = "white", linewidth = .3) +
    scale_fill_manual(values = fills, name = block$legend, drop = FALSE) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(title = block$title, y = "Item", x = if (show == "fill") "Share of students" else "Number of students")
  if (show == "fill") p <- p + scale_x_continuous(labels = scales::percent_format(), expand = c(0, 0))
  if (split) p <- p + facet_wrap(~status)
  p
}

# lighter green for the two green value boxes (dark text stays readable)
light_green <- value_box_theme(bg = "#8ED1C8", fg = cfg$colours[["ink"]])

# ------------------------------------------------------------------- UI ----
ui <- page_navbar(
  title = cfg$title, window_title = paste(cfg$title, "| STA304"),
  theme = app_theme, fillable = FALSE, lang = "en",

  sidebar = sidebar(
    width = 270, open = "desktop",
    div(class = "counter", textOutput("n_selected")),
    accordion(
      open = TRUE,  # all filter panels start open
      accordion_panel("Student status", value = "status",
        checkboxGroupInput("f_status", NULL, choices = levels(survey$status), selected = levels(survey$status))),
      accordion_panel("cGPA band", value = "cgpa",
        checkboxGroupInput("f_cgpa", NULL, choices = c(cgpa_levels, "Not reported"),
                           selected = c(cgpa_levels, "Not reported"))),
      accordion_panel("AI use frequency", value = "ai",
        sliderInput("f_ai", NULL, min = 1, max = 10, value = c(1, 10), step = 1, ticks = FALSE)),
      accordion_panel("Chart colours", value = "colours",
        selectInput("palette", NULL, choices = names(palettes)))
    ),
    actionButton("reset", "Reset filters", class = "btn-outline-primary btn-sm")
  ),

  nav_panel("Overview", page_body(
    layout_columns(
      fill = FALSE, col_widths = breakpoints(sm = c(6, 6), lg = c(3, 3, 3, 3)),
      value_box("Students", textOutput("vb_n"), theme = "primary"),
      value_box("International", textOutput("vb_intl"), theme = light_green),
      value_box("Average AI use frequency (1\u201310)", textOutput("vb_ai"), theme = "primary"),
      value_box("Typical hours per week AI saves", textOutput("vb_saved"), theme = light_green)
    ),
    analysis_card("Explore one question at a time", "ov_plot", "ov_interp", height = 340,
      controls = ctrl_row(
        selectInput("ov_var", "Question", choices = num_labels, selected = "ai_use_frequency"),
        uiOutput("ov_xlim_ui"),
        uiOutput("ov_bins_ui")
      )),
    analysis_card("Who answered?", "ov_profile", "ov_profile_interp", height = 320)
  )),

  nav_panel("RQ1: Activities", page_body(
    analysis_card(textOutput("r1_header", inline = TRUE), "r1_plot", "r1_interp",  # header follows the Topic choice
      height = 470,
      controls = ctrl_row(
        selectInput("r1_block", "Topic", choices = block_choices, selected = "replace"),
        radioButtons("r1_show", "Show", choices = c("Percent" = "fill", "Count" = "stack"), inline = TRUE),
        selectInput("r1_split", "Split by", choices = c("Nobody" = "none", "Student status" = "status"))
      ),
      details = tagList(tableOutput("r1_table"),
        p(class = "small-note", "Each row tests whether the share answering at the level shown differs from 50%. Adjusted p-values use the Holm correction to account for testing several items at once; items are not compared with each other."))
    )
  )),

  nav_panel("RQ2: Domestic vs international", page_body(
    navset_card_pill(
      title = "Do international and domestic students differ in how they use AI?",
      nav_panel("Proportions", analysis_body("r2p_plot", "r2p_interp", height = 430,
        controls = ctrl_row(
          selectInput("r2_block", "Topic", choices = block_choices, selected = "replace"),
          sliderInput("r2_xlim", "Horizontal range", min = 0, max = 1, value = c(0, 1), step = 0.05, ticks = FALSE)),
        details = tableOutput("r2p_table"))),
      nav_panel("Averages", analysis_body("r2m_plot", "r2m_interp", height = 460,
        controls = ctrl_row(
          selectInput("r2_var", "Measure", choices = c("All four side by side" = "all", mean_labels), selected = "all"),
          radioButtons("r2_type", "Style", choices = c("Box" = "box", "Violin" = "violin"), inline = TRUE),
          # heading on the same line as MEASURE and STYLE, then the checkbox
          div(class = "form-group", tags$label(class = "control-label", "Students"),
              checkboxInput("r2_points", "Show each student as a dot", TRUE)),
          uiOutput("r2_ylim_ui")),
        details = tableOutput("r2m_table"))),
      footer = p(class = "small-note", "The student-status filter is ignored on this tab (otherwise there is nothing to compare); the other filters still apply.")
    )
  )),

  nav_panel("RQ3: Confidence", page_body(
    analysis_card("Which factors are linked to confidence without AI?", "r3_coef_plot", "r3_interp",
      height = 330,
      controls = ctrl_row(
        checkboxGroupInput("r3_preds", "Factors to include", choices = pred_labels, selected = unname(pred_labels), inline = TRUE),
        sliderInput("r3_xlim_coef", "Horizontal range", min = -5, max = 5, value = c(-2, 2), step = 0.5, ticks = FALSE),
        widths = c(8, 4)),
      details = tableOutput("r3_table")),
    analysis_card("Look at one factor on its own", "r3_scatter", "r3_scatter_interp", height = 430,
      controls = ctrl_row(
        selectInput("r3_x", "Factor", choices = pred_labels, selected = "ai_use_frequency"),
        uiOutput("r3_xlim_ui"),
        div(checkboxInput("r3_trend", "Trend line", TRUE), checkboxInput("r3_split", "Colour by status", FALSE))))
  )),

  nav_panel("RQ4: Consistency", page_body(
    analysis_card("Do the questions in each set move together?", "r4_plot", "r4_interp", height = 340,
      controls = ctrl_row(
        checkboxInput("r4_split", "Compare student status", FALSE),
        sliderInput("r4_ylim", "Vertical range", min = 0, max = 1, value = c(0, 1), step = 0.05, ticks = FALSE)),
      details = tableOutput("r4_table")),
    analysis_card("How strongly do the individual items move together?", "r4_heat", "r4_heat_interp", height = 430,
      controls = ctrl_row(selectInput("r4_block", "Topic", choices = block_choices, selected = "importance")))
  )),

  # ---------------------------------------------------------------- DATA ----
  nav_panel(
    "Data",
    card(
      card_header(
        "Filtered responses (sort or search)",
        downloadButton("dl", "Download CSV", class = "btn-sm btn-outline-primary float-end")
      ),
      DTOutput("tbl")
    )
  ),

  # --------------------------------------------------------------- ABOUT ----
  nav_panel(
    "About",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("About this survey"),
        p("This app presents the results of our STA304 survey on how university students use generative AI
           tools (such as chatbots) for academic work. Respondents told us how often they use AI, what
           for, why, how much time they think it saves, how many hours they study, and how confident they
           feel doing their work without it."),
        p("The survey was a Google Form shared with STA304H5 (Fall 2026) students through Piazza, and taking part was voluntary.
           The data contain ", strong(nrow(survey)), " respondents after keeping only the latest submission for repeated course IDs.
           The data file this app uses does not include those course IDs."),
        h6("Caveats to keep in mind"),
        tags$ul(
          tags$li("Not a random sample: students chose to respond, so results describe these respondents and may not represent all STA304 or university students."),
          tags$li("Everything is self-reported, so answers may be rounded, misremembered or influenced by how people want to appear."),
          tags$li("Some students gave their hours as a range instead of a single number (15 for weekly study hours and 14 for hours per week AI saves); we used the midpoint, and for \u201c<2 hours\u201d we used 1. Open-ended answers (e.g. \u201c15+ hours\u201d) were treated as missing: 4 students for weekly study hours, 4 for hours per week AI saves, and 2 gave no cGPA."),
          tags$li("cGPA was collected in bands (e.g. 3.0\u20133.29), so each student is placed at the middle of their band. For \u201cBelow 2.7\u201d we assume the band starts at 0, so those ", sum(round(survey$cgpa_midpoint, 3) == 1.35, na.rm = TRUE), " students are placed at 1.35, the same choice as in our EDA report. A different lower bound would change any result that uses cGPA, so read those results with caution."),
          tags$li("The 1\u201310 scores come straight from the survey; the export does not record what 1 and 10 stood for. We read higher numbers as \u201cmore\u201d (more frequent, more difficult, more confident)."),
          tags$li("With about 130 students, small differences can easily be chance. Filtering to small subgroups makes results shakier still."),
          tags$li("The tests and tables count a student as replacing or using something if they answered at least \u201cSometimes\u201d, and as rating a reason important if they answered at least \u201cModerately Important\u201d."),
          tags$li("The regression assumes equal spread of errors; in our full sample this was borderline (Breusch\u2013Pagan p \u2248 0.049), so treat p-values as approximate."),
          tags$li("Cronbach\u2019s alpha says whether items move together, not that they measure one underlying idea."),
          tags$li("We run several tests across tabs, so a single p-value just under 0.05 should be treated cautiously.")
        )
      ),
      card(
        card_header("How to read the results"),
        accordion(
          open = FALSE,
          accordion_panel("Box and violin plots",
            "The box covers the middle half of students and the line inside is the median (the typical student).
             The diamond is the average. A violin is a mirrored histogram: wider means more students at that value."),
          accordion_panel("Regression effects",
            "Regression estimates how much confidence changes when one factor goes up by one unit while the others are held fixed.
             If the 95% confidence interval around an effect crosses 0, we cannot rule out no effect."),
          accordion_panel("\u03b1 (Cronbach\u2019s alpha)",
            "A score, usually between 0 and 1, for how consistently a set of questions moves together. Above 0.7 is usually called acceptable. It can be below 0 when the questions do not move together at all."),
          accordion_panel("p-value",
            "How surprising our result would be if there were really no pattern in the wider population.
             Small values (below 0.05) mean the pattern is unlikely to be a fluke."),
          accordion_panel("t-tests and proportion tests",
            "A t-test asks whether two groups\u2019 averages differ by more than chance. Proportion tests ask the same about percentages
             (Fisher\u2019s exact test is used when some counts are very small)."),
          accordion_panel("Holm correction",
            "A method that reduces the chance of false positive results when conducting multiple statistical tests."),
          accordion_panel("Adjusted R\u00b2",
            "Measures how well the model explains the data while accounting for the number of predictors used."),
          accordion_panel("Welch\u2019s t-test",
            "Compares two group means without assuming that the groups have equal variances."),
          accordion_panel("Pooled t-test",
            "Compares two group means assuming that the groups have equal variances."),
          accordion_panel("Breusch\u2013Pagan test",
            "Checks whether the variability of the model\u2019s errors remains consistent."),
          accordion_panel("95% confidence interval",
            "A range of plausible values for the true population value, estimated with 95% confidence."),
          accordion_panel("Correlation (r)",
            "A value between \u22121 and 1 that shows the strength and direction of the linear relationship between two variables, with 0 indicating no linear relationship.")
        )
      )
    )
  )
)

# ---------------------------------------------------------------- SERVER ----
server <- function(input, output, session) {

  # ---- shared helpers ----
  pal <- reactive(palettes[[input$palette]])
  seq_cols <- function(n) colorRampPalette(c("#FFFFFF", pal()[1]))(n + 2)[-(1:2)]
  plot_theme <- reactive({
    theme_minimal(base_size = cfg$base_size) +
      theme(plot.title = element_text(face = "bold", size = rel(1.15)), plot.title.position = "plot",
            plot.subtitle = element_text(colour = cfg$colours[["muted"]], margin = margin(b = 8)),
            axis.text = element_text(colour = "#3B4756"), legend.position = "bottom",
            panel.grid.minor = element_blank(), plot.margin = margin(10, 14, 6, 10))
  })
  need_n <- function(d, n = 5)
    validate(need(nrow(d) >= n, "Too few students match the current filters \u2013 widen them in the sidebar."))

  filtered <- function(ignore_status = FALSE) {
    d <- survey
    if (!ignore_status) d <- d[d$status %in% input$f_status, ]
    keep_g <- (as.character(d$cgpa_band) %in% input$f_cgpa) |
      (is.na(d$cgpa_band) & "Not reported" %in% input$f_cgpa)
    d[keep_g & d$ai_use_frequency >= input$f_ai[1] & d$ai_use_frequency <= input$f_ai[2], ]
  }
  dat    <- reactive(filtered())
  dat_ns <- reactive(filtered(TRUE))

  output$n_selected <- renderText(sprintf("%d of %d students", nrow(dat()), nrow(survey)))

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "f_status", selected = levels(survey$status))
    updateCheckboxGroupInput(session, "f_cgpa", selected = c(cgpa_levels, "Not reported"))
    updateSliderInput(session, "f_ai", value = c(1, 10))
  })

  # ------------------------------------------------------------ OVERVIEW ----
  output$vb_n    <- renderText(nrow(dat()))
  output$vb_intl <- renderText({
    d <- dat(); if (nrow(d) == 0) return("\u2013")
    sprintf("%.0f%%", 100 * mean(d$status == "International"))
  })
  output$vb_ai <- renderText({
    d <- dat(); if (nrow(d) == 0) return("\u2013")
    sprintf("%.1f", mean(d$ai_use_frequency))
  })
  output$vb_saved <- renderText({
    d <- dat(); if (nrow(d) == 0 || all(is.na(d$ai_hours_saved_week))) return("\u2013")
    sprintf("%.1f hrs", median(d$ai_hours_saved_week, na.rm = TRUE))
  })

  output$ov_xlim_ui <- renderUI({
    req(input$ov_var)
    r <- range(survey[[input$ov_var]], na.rm = TRUE)
    lo <- floor(r[1]); hi <- ceiling(r[2])
    sliderInput("ov_xlim", "Zoom horizontal axis", min = lo, max = hi, value = c(lo, hi),
                step = 1, ticks = FALSE)
  })

  score_vars <- c("ai_use_frequency", "confidence_without_ai", "coursework_difficulty")  # whole-number 1-10 scores
  output$ov_bins_ui <- renderUI({
    req(input$ov_var)
    if (input$ov_var %in% score_vars) return(NULL)  # one bar per score, so no bar slider
    sliderInput("ov_bins", "Bars", min = 5, max = 40, value = 15, ticks = FALSE)
  })

  ov_values <- reactive({
    d <- dat(); need_n(d)
    x <- d[[input$ov_var]]; x[!is.na(x)]
  })

  output$ov_plot <- renderPlot({
    req(input$ov_xlim)
    x <- ov_values()
    ggplot(data.frame(x = x), aes(x)) +
      (if (input$ov_var %in% score_vars)
         geom_histogram(binwidth = 1, center = 0, fill = pal()[1], colour = "white")  # bars centred on 1, 2, ..., 10
       else
         geom_histogram(bins = if (is.null(input$ov_bins)) 15 else input$ov_bins, fill = pal()[1], colour = "white")) +
      geom_vline(xintercept = median(x), linetype = "dashed", colour = "#E8202A", linewidth = 1.1) +  # bright red median line
      (if (input$ov_var %in% score_vars) scale_x_continuous(breaks = 1:10)) +
      coord_cartesian(xlim = input$ov_xlim + if (input$ov_var %in% score_vars) c(-0.5, 0.5) else 0) +
      labs(title = paste("How answers are spread:", label_of(input$ov_var)),
           subtitle = "Red dashed line = typical (median) student",
           x = label_of(input$ov_var), y = "Number of students") +
      plot_theme()
  })

  output$ov_interp <- renderUI({
    x <- ov_values(); q <- quantile(x, c(.25, .5, .75))
    interp_box(sprintf("In this selection, the typical student's answer for %s is %s. About half of students fall between %s and %s (based on %d answers).",
                       short_of(input$ov_var), format(round(q[2], 1)), format(round(q[1], 1)),
                       format(round(q[3], 1)), length(x)),
               if (input$ov_var == "cgpa_midpoint") cgpa_note)
  })

  profile_data <- reactive({
    d <- dat(); need_n(d, 1)
    d |>
      mutate(band = factor(coalesce(as.character(cgpa_band), "Not reported"),
                           levels = c(cgpa_levels, "Not reported"))) |>
      count(band, status, .drop = FALSE)
  })

  output$ov_profile <- renderPlot({
    ggplot(profile_data(), aes(n, band, fill = status)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = pal()[1:2], drop = FALSE, name = "Student status") +
      scale_y_discrete(limits = rev) +
      labs(title = "Respondents by cGPA band", x = "Number of students", y = "cGPA band") +
      plot_theme()
  })

  output$ov_profile_interp <- renderUI({
    d <- profile_data(); tot <- sum(d$n)
    top <- d |> group_by(band) |> summarise(n = sum(n)) |> arrange(desc(n)) |> slice(1)
    interp_box(sprintf("Of the %d students selected, the largest group (%d students) %s.", tot, top$n,
                       if (top$band == "Not reported") "did not report a cGPA" else paste("reported a cGPA of", top$band)))
  })

  # ----------------------------------------------------------------- RQ1 ----
  r1_block <- reactive(likert_blocks[[input$r1_block]])
  output$r1_header <- renderText(r1_block()$question)

  r1_long <- reactive({
    d <- dat(); need_n(d); b <- r1_block()
    d |>
      select(status, all_of(unname(b$vars))) |>
      pivot_longer(-status, names_to = "item", values_to = "score") |>
      filter(!is.na(score)) |>
      mutate(item = names(b$vars)[match(item, b$vars)],
             score = factor(score, levels = 1:5, labels = b$levels))
  })
  r1_share <- reactive({
    r1_long() |> group_by(item) |>
      summarise(share = mean(as.integer(score) >= 3), .groups = "drop") |> arrange(share)
  })

  output$r1_plot <- renderPlot({
    likert_bar(r1_long(), r1_block(), r1_share()$item, seq_cols(5), input$r1_show, input$r1_split == "status") +
      plot_theme()
  })

  r1_tab <- reactive({
    d <- dat(); need_n(d); b <- r1_block()
    out <- do.call(rbind, lapply(names(b$vars), function(nm) {
      r <- prop_row(d[[b$vars[[nm]]]])
      data.frame(Item = nm, n = r[["n"]], p = r[["p"]], lo = r[["lo"]], hi = r[["hi"]], pval = r[["pval"]])
    }))
    out$padj <- p.adjust(out$pval, "holm")
    out
  })

  output$r1_table <- renderTable({
    t <- r1_tab(); b <- r1_block()
    data.frame(Item = t$Item, Students = as.integer(t$n),  # whole numbers, not "130.00"
               `Share at this level` = pct(t$p),
               `95% confidence interval` = paste0(pct(t$lo), " \u2013 ", pct(t$hi)),
               `Adjusted p-value` = vapply(t$padj, fmt_p, ""), check.names = FALSE) |>
      setNames(c("Item", "Students", paste("Share", b$atleast), "95% confidence interval", "Adjusted p-value"))
  })

  output$r1_interp <- renderUI({
    b <- r1_block(); sh <- r1_share(); t <- r1_tab()
    top <- sh[nrow(sh), ]; bot <- sh[1, ]
    n_sig <- sum(t$padj < 0.05 & t$p > 0.5); n_low <- sum(t$padj < 0.05 & t$p < 0.5)
    interp_box(sprintf("The most widely endorsed item is \u201c%s\u201d: %.0f%% of students %s. The least endorsed is \u201c%s\u201d (%.0f%%). After adjusting for multiple tests, for %d of %d items more than half of students answered %s%s. Darker bars mean stronger answers.",
                       top$item, 100 * top$share, b$topword, bot$item, 100 * bot$share, n_sig, nrow(t), b$atleast,
                       if (n_low > 0) sprintf("; for %d, fewer than half did", n_low) else ""))
  })

  # ----------------------------------------------------------------- RQ2 ----
  r2p_data <- reactive({
    d <- dat_ns(); need_n(d); b <- likert_blocks[[input$r2_block]]
    long <- d |>
      select(status, all_of(unname(b$vars))) |>
      pivot_longer(-status, names_to = "item", values_to = "score") |>
      filter(!is.na(score)) |>
      mutate(item = names(b$vars)[match(item, b$vars)]) |>
      group_by(item, status) |>
      summarise(n = n(), k = sum(score >= 3), p = k / n, .groups = "drop")
    validate(need(length(unique(long$status)) == 2 && all(long$n >= 3),
                  "Each group needs at least 3 students \u2013 widen the filters."))
    long
  })

  output$r2p_plot <- renderPlot({
    d <- r2p_data(); b <- likert_blocks[[input$r2_block]]
    ord <- d |> group_by(item) |> summarise(m = mean(p)) |> arrange(m)
    d$item <- factor(d$item, levels = ord$item)
    ggplot(d, aes(p, item, colour = status, shape = status)) +
      geom_point(size = 4, alpha = .9) +
      scale_colour_manual(values = pal()[1:2], name = "Student status") +
      scale_shape_manual(values = c(16, 17), name = "Student status") +
      scale_x_continuous(labels = scales::percent_format(), limits = input$r2_xlim, oob = scales::oob_keep) +
      labs(title = b$title, subtitle = paste("Share of students answering", b$atleast),
           x = "Share of students in each group", y = "Item") +
      plot_theme()
  })

  r2p_tab <- reactive({
    d <- r2p_data()
    do.call(rbind, lapply(unique(d$item), function(it) {
      x <- d[d$item == it, ]
      D <- x[x$status == "Domestic", ]; I <- x[x$status == "International", ]
      r <- two_prop_p(D$k, D$n, I$k, I$n)
      data.frame(Item = it, Domestic = pct(D$p), International = pct(I$p), gap = D$p - I$p,
                 p = r[1], Test = if (r[2] == 1) "Fisher\u2019s exact" else "Two-proportion")
    }))
  })

  output$r2p_table <- renderTable({
    t <- r2p_tab()
    data.frame(Item = t$Item, Domestic = t$Domestic, International = t$International,
               `p-value` = vapply(t$p, fmt_p, ""), Test = t$Test, check.names = FALSE)
  })

  output$r2p_interp <- renderUI({
    t <- r2p_tab(); sig <- t[!is.na(t$p) & t$p < 0.05, ]; big <- t[which.max(abs(t$gap)), ]
    interp_box(sprintf("%d of %d items show a gap between domestic and international students that is larger than chance alone would usually produce (unadjusted, 5%% level)%s. The biggest gap is for \u201c%s\u201d: %s of domestic versus %s of international students. With many items tested, one or two small p-values can appear by luck.",
                       nrow(sig), nrow(t),
                       if (nrow(sig) > 0) paste0(": ", paste(sig$Item, collapse = ", ")) else "",
                       big$Item, big$Domestic, big$International))
  })

  output$r2_ylim_ui <- renderUI({
    req(input$r2_var); if (input$r2_var == "all") return(NULL)
    r <- range(survey[[input$r2_var]], na.rm = TRUE)
    lo <- floor(r[1]); hi <- ceiling(r[2])
    sliderInput("r2_ylim", "Vertical range", min = lo, max = hi, value = c(lo, hi), step = 1, ticks = FALSE)
  })

  r2m_vars <- reactive(if (input$r2_var == "all") unname(mean_labels) else input$r2_var)

  r2m_data <- reactive({
    d <- dat_ns(); need_n(d)
    validate(need(all(table(d$status) >= 3), "Each group needs at least 3 students \u2013 widen the filters."))
    d
  })

  output$r2m_plot <- renderPlot({
    vs <- r2m_vars()
    group_box_plot(r2m_data(), vs, num_labels, "status", pal()[1:2], input$r2_type, input$r2_points,
                   ylim = if (length(vs) == 1) input$r2_ylim else NULL) +
      labs(title = if (length(vs) == 1) paste(label_of(vs), "by student status") else "How domestic and international students compare",
           subtitle = paste0("Diamond = average; line = median", if (input$r2_points) "; grey dots = individual students")) +
      plot_theme()
  })

  r2m_tests <- reactive({
    d <- r2m_data()
    # a t-test cannot run if everyone in a group gave the same answer (e.g. AI use slider set to one value)
    flat <- Filter(function(v) any(tapply(d[[v]], d$status, function(x) length(unique(na.omit(x))) < 2)), r2m_vars())
    validate(need(length(flat) == 0, paste0(
      "Everyone in at least one group gives the same answer for ", paste(vapply(flat, short_of, ""), collapse = ", "),
      ", so the averages can't be tested. Widen the filters or pick another measure.")))
    do.call(rbind, lapply(r2m_vars(), function(v) compare_means(d, v)))
  })

  output$r2m_table <- renderTable({
    t <- r2m_tests()
    data.frame(Measure = label_of(t$id), Domestic = sprintf("%.2f", t$m1), International = sprintf("%.2f", t$m2),
               Difference = sprintf("%.2f", t$diff), `95% confidence interval` = sprintf("%.2f to %.2f", t$lo, t$hi),
               `p-value` = vapply(t$p, fmt_p, ""), Test = t$test, check.names = FALSE)
  })

  output$r2m_interp <- renderUI({
    t <- r2m_tests()
    if (nrow(t) == 1) {
      ev <- if (t$p < 0.05) "That gap is bigger than chance alone would usually produce, so there is statistical evidence the group averages differ."
            else "That gap is small enough to happen by chance, so we do not have enough evidence the group averages differ (this does not prove they are equal)."
      return(interp_box(sprintf("Domestic students average %.2f on %s versus %.2f for international students, a difference of %.2f (%s). %s",
                                t$m1, short_of(t$id), t$m2, t$diff, fmt_p(t$p), ev)))
    }
    sig <- t[t$p < 0.05, ]; best <- t[which.min(t$p), ]
    interp_box(sprintf("%d of %d measures show a difference in averages larger than chance would usually produce (5%% level)%s. The strongest signal is for %s: domestic %.2f vs international %.2f (%s). Heavily overlapping boxes mean the two groups look very similar.",
                       nrow(sig), nrow(t), if (nrow(sig) > 0) paste0(": ", paste(vapply(sig$id, short_of, ""), collapse = ", ")) else "",
                       short_of(best$id), best$m1, best$m2, fmt_p(best$p)))
  })

  # ----------------------------------------------------------------- RQ3 ----
  r3_model <- reactive({
    preds <- input$r3_preds
    validate(need(length(preds) >= 1, "Pick at least one factor."))
    d <- dat(); dd <- d[complete.cases(d[, c("confidence_without_ai", preds)]), ]
    validate(need(nrow(dd) >= length(preds) + 15, "Too few students for this model \u2013 widen the filters or pick fewer factors."))
    validate(need(all(vapply(dd[preds], \(x) sd(x) > 0, TRUE)), "One of the chosen factors has no variation in this selection."))
    list(fit = lm(reformulate(preds, "confidence_without_ai"), data = dd), n = nrow(dd))
  })

  r3_coefs <- reactive({
    m <- r3_model(); cf <- summary(m$fit)$coefficients; ci <- confint(m$fit)
    data.frame(term = rownames(cf), Estimate = cf[, 1], lo = ci[, 1], hi = ci[, 2], p = cf[, 4], row.names = NULL) |>
      filter(term != "(Intercept)") |>
      mutate(Factor = label_of(term), sig = p < 0.05)
  })

  output$r3_coef_plot <- renderPlot({
    cf <- r3_coefs()
    ggplot(cf, aes(Estimate, reorder(Factor, Estimate), colour = sig)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#8C8C9E") +
      geom_pointrange(aes(xmin = lo, xmax = hi), linewidth = 1, size = .7) +
      scale_colour_manual(values = c(`TRUE` = pal()[1], `FALSE` = "#9A9AA8"),
                          labels = c(`TRUE` = "Clear evidence (p < 0.05)", `FALSE` = "Not clear"), name = "Strength of evidence") +
      coord_cartesian(xlim = input$r3_xlim_coef) +
      labs(title = "Link between each factor and confidence without AI",
           subtitle = "Dot = estimated change in confidence (1\u201310) per one-unit increase; bar = 95% confidence interval",
           x = "Change in confidence score", y = "Factor") +
      plot_theme()
  })

  output$r3_table <- renderTable({
    cf <- r3_coefs()
    data.frame(Factor = cf$Factor, Effect = sprintf("%.2f", cf$Estimate),
               `95% confidence interval` = sprintf("%.2f to %.2f", cf$lo, cf$hi), `p-value` = vapply(cf$p, fmt_p, ""),
               check.names = FALSE)
  })

  output$r3_interp <- renderUI({
    m <- r3_model(); cf <- r3_coefs(); sm <- summary(m$fit); f <- sm$fstatistic
    pm <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    sig <- cf[cf$sig, ]
    sig_txt <- if (nrow(sig) == 0) "No single factor stands out clearly."
    else paste(sprintf("Each %s goes with %.2f points %s confidence (%s).",
                       c(study_hours_week = "extra hour of weekly study", ai_hours_saved_week = "extra hour that AI saves per week",
                         coursework_difficulty = "extra point of coursework difficulty", ai_use_frequency = "extra point of AI use frequency",
                         cgpa_midpoint = "extra grade point of estimated cGPA")[sig$term], abs(sig$Estimate),
                       ifelse(sig$Estimate < 0, "lower", "higher"), vapply(sig$p, fmt_p, "")), collapse = " ")
    # adjusted R-squared can drop below 0 when the factors explain almost nothing
    expl <- if (sm$adj.r.squared < 0.005) "the chosen factors explain almost none of the differences in confidence (adjusted R\u00b2 is about 0)"
            else sprintf("the chosen factors together explain about %.0f%% of the differences in confidence (adjusted R\u00b2)", 100 * sm$adj.r.squared)
    interp_box(sprintf("Using %d students, %s, and the model overall is %s (%s). %s These are associations, not proof that one thing causes another, and most of the variation in confidence is still unexplained.",
                       m$n, expl, if (pm < 0.05) "statistically clear" else "not statistically clear",
                       fmt_p(pm), sig_txt),
               if ("cgpa_midpoint" %in% input$r3_preds) cgpa_note)
  })

  output$r3_xlim_ui <- renderUI({
    req(input$r3_x)
    r <- range(survey[[input$r3_x]], na.rm = TRUE)
    lo <- floor(r[1]); hi <- ceiling(r[2])
    sliderInput("r3_xlim", "Horizontal axis range", min = lo, max = hi, value = c(lo, hi), step = 1, ticks = FALSE)
  })

  output$r3_scatter <- renderPlot({
    req(input$r3_xlim)
    d <- dat(); need_n(d); x <- input$r3_x
    d <- d[!is.na(d[[x]]), ]
    # no jitter: points stay at the reported values; a bigger dot = more students gave that answer
    if (input$r3_split) {
      p <- ggplot(d, aes(.data[[x]], confidence_without_ai, colour = status)) +
        geom_count(alpha = .75) +
        scale_colour_manual(values = pal()[1:2], name = "Student status")
      if (input$r3_trend) p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
    } else {
      p <- ggplot(d, aes(.data[[x]], confidence_without_ai)) +
        geom_count(alpha = .75, colour = pal()[1])
      if (input$r3_trend)
        p <- p + geom_smooth(method = "lm", formula = y ~ x, colour = pal()[2], fill = pal()[2], alpha = .15)
    }
    p + coord_cartesian(xlim = input$r3_xlim, ylim = c(1, 10)) +
      scale_y_continuous(breaks = 1:10) +
      scale_size_area(max_size = 9, name = "Number of students") +
      labs(title = paste("Confidence without AI vs.", short_of(x)),
           x = label_of(x), y = label_of("confidence_without_ai")) +
      plot_theme() + theme(legend.box = "vertical")  # stack the two legends so they fit on narrow screens
  })

  output$r3_scatter_interp <- renderUI({
    d <- dat(); need_n(d); x <- input$r3_x
    d <- d[!is.na(d[[x]]) & !is.na(d$confidence_without_ai), ]
    validate(need(nrow(d) >= 5 && sd(d[[x]]) > 0 && sd(d$confidence_without_ai) > 0,
                  "Not enough variation in this selection to describe the relationship."))
    r <- cor(d[[x]], d$confidence_without_ai)
    who <- c(ai_use_frequency = "those who use AI more often", study_hours_week = "those who study more hours each week",
             coursework_difficulty = "those who find their coursework more difficult",
             ai_hours_saved_week = "those who say AI saves them more hours each week",
             cgpa_midpoint = "those with a higher estimated cGPA")[[x]]
    cause <- c(ai_use_frequency = "using AI more often", study_hours_week = "studying more hours",
               coursework_difficulty = "finding coursework more difficult", ai_hours_saved_week = "AI saving more time",
               cgpa_midpoint = "a higher cGPA")[[x]]
    r_txt <- sub("^-", "\u2212", sprintf("%.2f", round(r, 2) + 0))
    common <- "Each dot represents students who gave the same answers, and larger dots represent more students. This chart only looks at the relationship between two variables, while the model above also considers other factors."
    txt <- if (abs(r) < 0.1)
      sprintf("Among the %d students, there is almost no relationship between %s and confidence when working without AI (r = %s, almost no correlation). %s The results do not show a clear relationship.",
              nrow(d), short_of(x), r_txt, common)
    else
      sprintf("Among the %d students, %s tend to feel %s confident when working without AI (r = %s, %s %s correlation). %s The results show a relationship but do not prove that %s %s confidence.",
              nrow(d), who, if (r < 0) "less" else "more", r_txt, strength_word(r), if (r < 0) "negative" else "positive",
              common, cause, if (r < 0) "reduces" else "increases")
    interp_box(txt, if (x == "cgpa_midpoint") cgpa_note)
  })

  # ----------------------------------------------------------------- RQ4 ----
  r4_alpha <- reactive({
    d <- dat(); need_n(d)
    groups <- if (input$r4_split) split(d, d$status) else list(All = d)
    out <- do.call(rbind, lapply(names(groups), function(g) {
      do.call(rbind, lapply(names(alpha_groups), function(nm) {
        x <- groups[[g]][, unname(alpha_groups[[nm]])]
        data.frame(Group = nm, Status = g, Items = ncol(x), Students = sum(complete.cases(x)), alpha = cronbach(x))
      }))
    }))
    out$Group <- factor(out$Group, levels = names(alpha_groups))
    out
  })

  output$r4_plot <- renderPlot({
    d <- r4_alpha(); d <- d[!is.na(d$alpha), ]
    validate(need(nrow(d) > 0, "Not enough students to compute alpha."))
    p <- ggplot(d, aes(Group, alpha)) +
      geom_hline(yintercept = c(.7, .8), linetype = "dashed", colour = "#E8202A", linewidth = 0.8) +
      annotate("text", x = -Inf, y = c(.7, .8), label = c("acceptable", "good"), vjust = -0.5, hjust = 0, size = 3.5, colour = "#6b6a80")
    p <- if (input$r4_split)
      p + geom_col(aes(fill = Status), position = position_dodge(.8), width = .7) +
        geom_text(aes(label = sprintf("%.2f", alpha), group = Status, vjust = ifelse(alpha < 0, 1.4, -0.4)), position = position_dodge(.8), size = 3.8) +
        scale_fill_manual(values = pal()[1:2], name = "Student status")
    else
      p + geom_col(aes(fill = Group), width = .6, show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.2f", alpha), vjust = ifelse(alpha < 0, 1.4, -0.4)), size = 4) +
        scale_fill_manual(values = rep_len(pal(), 3))
    # a negative alpha is shown below 0 instead of being cut off
    p + coord_cartesian(ylim = c(min(input$r4_ylim[1], min(d$alpha) - 0.1), input$r4_ylim[2])) +
      scale_x_discrete(labels = scales::label_wrap(22)) +  # the full set names are long, so wrap them
      labs(title = "How consistently does each set of questions move together?", x = "Question set", y = "Cronbach\u2019s alpha") +
      plot_theme()
  })

  output$r4_table <- renderTable({
    d <- r4_alpha()
    data.frame(Group = as.character(d$Group), `Student status` = d$Status, Items = d$Items, Students = d$Students,
               Alpha = ifelse(is.na(d$alpha), "\u2013", sprintf("%.3f", d$alpha)),
               Rating = ifelse(is.na(d$alpha), "\u2013", alpha_label(d$alpha)), check.names = FALSE)[, if (input$r4_split) 1:6 else c(1, 3:6)]
  })

  output$r4_interp <- renderUI({
    d <- r4_alpha(); d <- d[!is.na(d$alpha), ]
    validate(need(nrow(d) > 0, ""))
    # one sentence per student group that has enough answers (a single "All" group when not split)
    txt <- vapply(split(d, d$Status), function(g)
      paste0(if (input$r4_split) paste0(g$Status[1], " students: ") else "",
             paste(sprintf("%s (\u03b1 = %.2f, %s)", g$Group, g$alpha, alpha_label(g$alpha)), collapse = "; ")), "")
    interp_box(sprintf("%s. Higher alpha means students who answer one question in a set a certain way tend to answer the others similarly. A high value does not prove the set measures one single idea. \u201cOther\u201d items are left out.",
                       paste(txt, collapse = ". ")))
  })

  output$r4_heat <- renderPlot({
    d <- dat(); need_n(d); b <- likert_blocks[[input$r4_block]]
    x <- d[, unname(b$vars)]; colnames(x) <- names(b$vars)
    cm <- cor(x, use = "pairwise.complete.obs")
    long <- as.data.frame(as.table(cm)); names(long) <- c("a", "b", "r")
    ggplot(long, aes(a, b, fill = r)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = ifelse(is.na(r), "\u2013", sprintf("%.2f", r))), size = 3.4) +
      scale_fill_gradient(low = "#FFFFFF", high = pal()[1], limits = c(min(0, min(long$r, na.rm = TRUE)), 1), name = "Correlation",
                          breaks = scales::breaks_pretty(n = 4), labels = scales::label_number(accuracy = 0.1),
                          guide = guide_colourbar(barwidth = grid::unit(14, "lines"),  # wider bar so the numbers do not overlap
                                                  # title on the same line as the bar, 1 cm to its left
                                                  theme = theme(legend.title.position = "left",
                                                                legend.title = element_text(vjust = 1, margin = margin(r = 1, unit = "cm"))))) +
      scale_y_discrete(limits = rev) +
      labs(title = b$title, x = "Item", y = "Item") +
      plot_theme() + theme(axis.text.x = element_text(angle = 35, hjust = 1))
  })

  output$r4_heat_interp <- renderUI({
    d <- dat(); need_n(d); b <- likert_blocks[[input$r4_block]]
    x <- d[, unname(b$vars)]; colnames(x) <- names(b$vars)
    cm <- suppressWarnings(cor(x, use = "pairwise.complete.obs"))
    pr <- which(upper.tri(cm) & !is.na(cm), arr.ind = TRUE)
    validate(need(nrow(pr) > 0, "Not enough variation in this selection to describe these relationships."))
    v <- cm[pr]; hi <- which.max(v); lo <- which.min(v); nm <- rownames(cm)
    num <- function(r) sub("^-", "\u2212", sprintf("%.2f", round(r, 2) + 0))
    words <- switch(input$r4_block,
      importance = c("two reasons for using AI", "Students who value one reason tend to value the other as well."),
      tasks      = c("two academic uses of AI", "Students who use AI for one tend to use it for the other as well."),
      replace    = c("two learning activities replaced by AI", "Students who use AI instead of one activity tend to do the same for the other."))
    interp_box(sprintf("Each square shows how strongly %s are related. A value closer to 1 means a stronger relationship, while a value closer to 0 means a weaker one. The strongest relationship is between \u201c%s\u201d and \u201c%s\u201d (%s). %s The weakest relationship is between \u201c%s\u201d and \u201c%s\u201d (%s). Darker squares indicate stronger relationships.",
                       words[1], nm[pr[hi, 1]], nm[pr[hi, 2]], num(v[hi]), words[2],
                       nm[pr[lo, 1]], nm[pr[lo, 2]], num(v[lo])))
  })

  # ---------------------------------------------------------------- DATA ----
  display_df <- reactive({
    d <- dat()
    # show the answer wording ("Never" ... "Very Often") instead of the 1-5 codes
    for (b in likert_blocks) for (v in b$vars) d[[v]] <- factor(d[[v]], levels = 1:5, labels = b$levels)
    d |> select(all_of(display_cols))
  })
  output$tbl <- renderDT({
    datatable(display_df(), rownames = FALSE, filter = "top",
              options = list(scrollX = TRUE, pageLength = 10, dom = "lfrtip"))
  })
  output$dl <- downloadHandler(
    filename = function() "filtered_survey_responses.csv",
    content = function(file) write_excel_csv(display_df(), file, na = "")  # Excel-friendly: keeps dashes readable
  )
}

shinyApp(ui, server)