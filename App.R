############ Canadian Internet Use Survey 2022 — App ############

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(srvyr)
library(dplyr)
library(rlang)
library(readxl)

## Load the pre-built survey design object ----
design <- readRDS("design_app.rds")

## Load the data dictionary ---- (created using survey codebook & claude)
dict <- as.data.frame(
  readxl::read_excel("dictionary.xlsx"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

dict_cols <- c(
  "Category", "Variable Name", "Concept", "Question Text",
  "Answer Categories", "Universe", "Note"
)
dict <- dict[, dict_cols, drop = FALSE]

dict_categories <- unique(dict[["Category"]])

## Variable choices ----
weight_cols <- c("WTPG", paste0("WRPG", 1:1000))
var_choices <- setdiff(names(design$variables), weight_cols)

## Estimate types (dropdown order) ----
estimate_types <- c(
  "Total"         = "total",
  "Mean"          = "mean",
  "Median"        = "median",
  "Quantile"      = "quantile",
  "Count"         = "count",
  "Proportion"    = "prop",
  "Correlation"   = "corr",
  "Ratio"         = "ratio",
  "Variance"      = "var",
  "Std deviation" = "sd"
)

categorical_types <- c("count", "prop")

## Types that need a second variable
two_var_types <- c("ratio", "corr")

## Types with no estimate quality rating
no_quality_types <- c("var", "sd", "corr")

operators <- c("=", ">=", "<=", ">", "<")

## Document links (PDFs saved in the www folder next to this app file) ----
## Paths are registered explicitly so they do not depend on how the app is launched.
www_dir <- file.path(getwd(), "www")
addResourcePath("docs", www_dir)

doc_links <- list(
  list(label = "User Manual",   file = "guidebook.pdf"),
  list(label = "Codebook",      file = "codebook.pdf"),
  list(label = "User Guide",    file = "userguide.pdf"),
  list(label = "Questionnaire", file = "questionnaire.pdf")
)

doc_links <- Filter(function(x) {
  ok <- file.exists(file.path(www_dir, x$file))
  if (!ok) {
    message(
      x$label, " NOT found at: ", file.path(www_dir, x$file),
      "\nCheck the folder is named 'www' and the file is exactly '", x$file,
      "' (Windows may hide a second '.pdf' extension)."
    )
  } else {
    message(x$label, " found: ", file.path(www_dir, x$file))
  }
  ok
}, doc_links)

## Estimate Quality classification ----
## unacceptable: n_unweighted < 15 OR cv > 0.333
## marginal:     n_unweighted >= 15 AND 0.166 <= cv <= 0.333
## acceptable:   n_unweighted >= 15 AND cv <= 0.165
classify_quality <- function(n_unwt, cv) {
  dplyr::case_when(
    n_unwt < 15 | cv > 0.333                 ~ "unacceptable",
    n_unwt >= 15 & cv > 0.165 & cv <= 0.333   ~ "marginal",
    n_unwt >= 15 & cv <= 0.165                ~ "acceptable",
    TRUE                                       ~ NA_character_
  )
}

## Legend text (shown below the results table, and appended to CSV) ----
quality_legend_lines <- c(
  "* Estimate Quality",
  "Acceptable: Unweighted n >= 15 and CV <= 0.165",
  "Marginal: Unweighted n >= 15 and CV 0.166-0.333 (Caution!)",
  "Unacceptable: Unweighted n < 15 or CV > 0.333 (Caution!!!)"
)

no_quality_note_line <- "Estimate Quality is not available for Variance, Standard Deviation or Correlation (no coefficient of variation computed)."

## Does this result contain a quality column? ----
has_quality_col <- function(df) {
  any(grepl("^estimate_quality", colnames(df)))
}

## One filter row (variable, operator, value) ----
## Rows 1-3 are visible at start; row 4 is revealed by "+ Add filter".
filter_row <- function(i) {
  
  cols <- list(
    column(
      3,
      pickerInput(
        paste0("filtervar", i),
        paste0("Filter Variable ", if (i > 1) paste0("(", i, ") ") else "", "(optional)"),
        choices = var_choices,
        options = pickerOptions(title = "None", maxOptions = 1, liveSearch = TRUE),
        multiple = TRUE,
        selected = character(0),
        width = "100%"
      )
    ),
    column(2, uiOutput(paste0("filterop", i, "_ui"))),
    column(3, uiOutput(paste0("filterval", i, "_ui")))
  )
  
  if (i == 1) {
    cols <- c(
      cols,
      list(
        column(
          4,
          tags$div(
            style = "text-align: right; margin-top: 30px; font-size: 12px; color: #666;",
            "Note: All filters are applied as an 'AND' condition."
          )
        )
      )
    )
  }
  
  row_args <- c(
    list(id = paste0("filter_row_", i)),
    if (i >= 3) list(style = "display: none;"),
    cols
  )
  
  do.call(fluidRow, row_args)
}

## UI ----
ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .dict-table-wrap {
        max-height: 360px;
        overflow-y: auto;
        border: 1px solid #ddd;
        border-radius: 4px;
      }
      .dict-table-wrap table {
        table-layout: fixed;
        width: 100%;
        margin-bottom: 0;
      }
      .dict-table-wrap th:nth-child(1), .dict-table-wrap td:nth-child(1) { width: 8%;  }
      .dict-table-wrap th:nth-child(2), .dict-table-wrap td:nth-child(2) { width: 9%;  }
      .dict-table-wrap th:nth-child(3), .dict-table-wrap td:nth-child(3) { width: 13%; }
      .dict-table-wrap th:nth-child(4), .dict-table-wrap td:nth-child(4) { width: 17%; }
      .dict-table-wrap th:nth-child(5), .dict-table-wrap td:nth-child(5) { width: 16%; }
      .dict-table-wrap th:nth-child(6), .dict-table-wrap td:nth-child(6) { width: 9%;  }
      .dict-table-wrap th:nth-child(7), .dict-table-wrap td:nth-child(7) { width: 28%; }
      .dict-table-wrap th, .dict-table-wrap td {
        white-space: pre-wrap;
        word-break: break-word;
        vertical-align: top;
        font-size: 12px;
      }
      .dict-table-wrap th {
        position: sticky;
        top: 0;
        background: #fff;
        z-index: 1;
      }

      /* Hidden boxes keep their space so Measures of Uncertainty never moves */
      .est-hidden { visibility: hidden; }
      .est-checks { display: flex; flex-wrap: wrap; gap: 14px; }
      .est-checks .form-group,
      .est-checks .shiny-input-container { margin-bottom: 0; width: auto !important; }
    "))
  ),
  
  titlePanel("Canadian Internet Use Survey 2022"),
  
  fluidRow(
    column(
      12,
      tags$div(
        style = "text-align: right; margin-bottom: -40px; position: relative; z-index: 2;",
        lapply(seq_along(doc_links), function(i) {
          x <- doc_links[[i]]
          tags$a(
            x$label,
            href = paste0("docs/", x$file),
            target = "_blank",
            rel = "noopener noreferrer",
            style = if (i < length(doc_links)) {
              "font-weight: 500; margin-right: 18px;"
            } else {
              "font-weight: 500;"
            }
          )
        })
      )
    )
  ),
  
  tabsetPanel(
    
    ## Data Dictionary tab ----
    tabPanel(
      "Data Dictionary",
      
      br(),
      
      fluidRow(
        column(
          4,
          pickerInput(
            "dict_category",
            "Category",
            choices = dict_categories,
            options = pickerOptions(
              title = "Select category",
              liveSearch = TRUE,
              actionsBox = TRUE
            ),
            multiple = TRUE,
            selected = character(0)
          )
        ),
        
        column(
          4,
          pickerInput(
            "dict_variable",
            "Variable Name",
            choices = character(0),
            options = pickerOptions(
              title = "Select category first",
              liveSearch = TRUE,
              actionsBox = TRUE,
              noneSelectedText = "Select category first"
            ),
            multiple = TRUE,
            selected = character(0)
          )
        )
      ),
      
      downloadButton("dict_export", "Download Dictionary"),
      
      br(), br(),
      
      fluidRow(
        column(
          12,
          tags$div(
            class = "dict-table-wrap",
            tableOutput("dict_table")
          )
        )
      )
    ),
    
    ## Descriptive Statistics tab ----
    tabPanel(
      "Descriptive Statistics",
      
      br(),
      
      ## Estimate settings (same column layout as the filters) ----
      fluidRow(
        column(
          3,
          selectInput(
            "esttype", "Estimate Type",
            choices = estimate_types, width = "100%"
          )
        ),
        column(
          3,
          ## Invisible (space kept) for Count and Proportion
          tags$div(
            id = "variable_wrap",
            pickerInput(
              "variable", "Estimate Variable",
              choices = var_choices,
              options = pickerOptions(
                title = "Select a variable", maxOptions = 1, liveSearch = TRUE
              ),
              multiple = TRUE, selected = character(0), width = "100%"
            )
          )
        ),
        column(
          3,
          ## Visible only for Ratio and Correlation (space always kept)
          tags$div(
            id = "variable2_wrap",
            class = "est-hidden",
            pickerInput(
              "variable2", "Second Variable",
              choices = var_choices,
              options = pickerOptions(
                title = "Select a variable", maxOptions = 1, liveSearch = TRUE
              ),
              multiple = TRUE, selected = character(0), width = "100%"
            )
          )
        ),
        column(
          3,
          tags$label("Measures of Uncertainty"),
          tags$div(
            class = "est-checks",
            checkboxInput("prec_ci", "95% CI", value = FALSE),
            checkboxInput("prec_se", "SE", value = FALSE),
            checkboxInput("prec_cv", "CV", value = FALSE),
            checkboxInput("show_n", "n_unweighted", value = FALSE)
          )
        )
      ),
      
      ## Optional grouping and filters ----
      hr(),
      
      ## Group by ----
      fluidRow(
        column(
          3,
          selectizeInput(
            "groupby", "Group By Variable (optional)",
            choices = var_choices, multiple = TRUE,
            selected = character(0), width = "100%",
            options = list(plugins = list("remove_button"))
          )
        )
      ),
      
      ## Filters (rows 1-3 visible; row 4 revealed by "+ Add filter") ----
      lapply(1:4, filter_row),
      
      fluidRow(
        column(
          3,
          actionButton("add_filter", "+ Add filter")
        )
      ),
      
      br(),
      
      ## Build table ----
      fluidRow(
        column(
          12,
          actionButton(
            "build",
            "Build Table",
            icon = icon("play")
          )
        )
      ),
      
      hr(),
      
      ## Results ----
      fluidRow(
        column(
          12,
          downloadButton("export", "Download Table"),
          br(), br(),
          tableOutput("result"),
          uiOutput("quality_legend_ui")
        )
      ),
      
      ## Footer ----
      fluidRow(
        column(
          12,
          tags$div(
            style = "text-align: right; margin-top: 10px; font-size: 12px; color: #666;",
            "Developed by Mayur Matang"
          )
        )
      )
      
    )
    
  )
)

## Server ----
server <- function(input, output, session) {
  
  ## Data Dictionary: fill Variable Name from selected categories ----
  observeEvent(input$dict_category, {
    cats <- input$dict_category
    
    if (length(cats) == 0) {
      updatePickerInput(
        session, "dict_variable",
        choices = character(0), selected = character(0)
      )
    } else {
      vars <- dict[["Variable Name"]][dict[["Category"]] %in% cats]
      keep <- intersect(input$dict_variable, vars)
      updatePickerInput(
        session, "dict_variable",
        choices = vars, selected = keep
      )
    }
  }, ignoreNULL = FALSE)
  
  output$dict_table <- renderTable({
    req(length(input$dict_variable) > 0)
    dict[dict[["Variable Name"]] %in% input$dict_variable, , drop = FALSE]
  },
  striped = TRUE, bordered = TRUE, rownames = FALSE, na = "", width = "100%")
  
  output$dict_export <- downloadHandler(
    filename = function() paste0("cius_data_dictionary_", Sys.Date(), ".csv"),
    content = function(file) write.csv(dict, file, row.names = FALSE)
  )
  
  ## Show/hide and relabel variable pickers based on estimate type ----
  ## Boxes are made invisible (not removed) so Measures of Uncertainty never moves.
  observeEvent(input$esttype, {
    type <- input$esttype
    
    ## Estimate Variable: invisible for Count and Proportion
    if (type %in% categorical_types) {
      shinyjs::addClass("variable_wrap", "est-hidden")
    } else {
      shinyjs::removeClass("variable_wrap", "est-hidden")
    }
    
    ## Second variable: visible only for Ratio and Correlation
    if (type %in% two_var_types) {
      shinyjs::removeClass("variable2_wrap", "est-hidden")
    } else {
      shinyjs::addClass("variable2_wrap", "est-hidden")
    }
    
    label1 <- switch(
      type,
      "ratio" = "Numerator Variable",
      "corr"  = "First Variable (X)",
      "Estimate Variable"
    )
    label2 <- switch(
      type,
      "ratio" = "Denominator Variable",
      "corr"  = "Second Variable (Y)",
      "Second Variable"
    )
    
    updatePickerInput(session, "variable", label = label1)
    updatePickerInput(session, "variable2", label = label2)
  }, ignoreNULL = FALSE)
  
  ## Is this column labelled (factor/character) rather than numeric? ----
  is_labelled <- function(varname) {
    col <- design$variables[[varname]]
    is.factor(col) || is.character(col)
  }
  
  ## Filters: 2 shown at start; "+ Add filter" reveals the 3rd, then the 4th ----
  n_filters <- reactiveVal(2)
  
  observeEvent(input$add_filter, {
    n <- n_filters() + 1
    shinyjs::show(paste0("filter_row_", n))
    n_filters(n)
    if (n >= 4) shinyjs::hide("add_filter")
  })
  
  lapply(1:4, function(i) {
    fvar_id <- paste0("filtervar", i)
    fop_id  <- paste0("filterop", i)
    fval_id <- paste0("filterval", i)
    
    output[[paste0(fop_id, "_ui")]] <- renderUI({
      req(length(input[[fvar_id]]) > 0)
      v <- input[[fvar_id]][1]
      if (is_labelled(v)) {
        selectInput(fop_id, "Operator", choices = "=")
      } else {
        selectInput(fop_id, "Operator", choices = operators)
      }
    })
    
    output[[paste0(fval_id, "_ui")]] <- renderUI({
      req(length(input[[fvar_id]]) > 0)
      v <- input[[fvar_id]][1]
      if (is_labelled(v)) {
        levs <- sort(unique(as.character(design$variables[[v]])))
        selectizeInput(fval_id, "Value(s)",
                       choices = levs, multiple = TRUE,
                       options = list(plugins = list("remove_button")),
                       width = "100%")
      } else {
        textInput(fval_id, "Value", value = "", width = "100%")
      }
    })
  })
  
  apply_filter <- function(d, fvar, fop, fval) {
    if (length(fvar) == 0 || is.null(fval) || length(fval) == 0 || all(fval == "")) {
      return(d)
    }
    v <- fvar[1]
    col <- sym(v)
    
    if (is_labelled(v)) {
      d %>% filter(as.character(!!col) %in% fval)
    } else if (fop == "=") {
      vals <- as.numeric(trimws(strsplit(fval, ",")[[1]]))
      d %>% filter(!!col %in% vals)
    } else {
      num_val <- as.numeric(fval)
      if (is.na(num_val)) return(d)
      switch(
        fop,
        ">=" = d %>% filter(!!col >= num_val),
        "<=" = d %>% filter(!!col <= num_val),
        ">"  = d %>% filter(!!col > num_val),
        "<"  = d %>% filter(!!col < num_val)
      )
    }
  }
  
  table_data <- eventReactive(input$build, {
    
    disable("build")
    on.exit(enable("build"), add = TRUE)
    
    type <- input$esttype
    
    vt <- c(
      if (isTRUE(input$prec_ci)) "ci",
      if (isTRUE(input$prec_se)) "se",
      if (isTRUE(input$prec_cv)) "cv"
    )
    
    ## Always compute cv internally for quality, regardless of checkbox
    vt_calc <- union(vt, "cv")
    
    ## Variance and correlation: no cv requested; "se" is always requested
    ## and dropped later if not ticked
    vt_se_ci <- union("se", intersect(vt, "ci"))
    
    d <- design
    
    for (i in seq_len(n_filters())) {
      fvar <- input[[paste0("filtervar", i)]]
      fop  <- input[[paste0("filterop", i)]]
      fval <- input[[paste0("filterval", i)]]
      d <- apply_filter(d, fvar, fop, fval)
    }
    
    ## Drop missing values (NA) in Group By variables ----
    gbys <- input$groupby
    
    for (g in gbys) {
      d <- d %>% filter(!is.na(!!sym(g)))
    }
    
    ## Count (tally) ----
    if (type == "count") {
      
      if (length(gbys) > 0) d <- d %>% group_by(across(all_of(gbys)))
      
      out <- d %>% survey_tally(vartype = vt_calc)
      
      n_tbl <- d %>% summarise(n_unweighted = unweighted(n()))
      out <- bind_cols(out, n_tbl["n_unweighted"])
      
      out <- out %>% mutate(estimate_quality = classify_quality(n_unweighted, n_cv))
      
      if (!isTRUE(input$show_n)) out <- out %>% select(-any_of("n_unweighted"))
      if (!isTRUE(input$prec_cv)) out <- out %>% select(-any_of("n_cv"))
      
      out <- out %>% rename(estimate_n = n)
      
      return(out)
    }
    
    ## Proportion ----
    if (type == "prop") {
      
      validate(need(length(gbys) > 0, "Proportion needs at least one Group by variable."))
      
      d <- d %>% group_by(across(all_of(gbys)))
      
      out <- d %>%
        summarise(
          estimate = survey_prop(vartype = vt_calc),
          n_unweighted = unweighted(n())
        )
      
      out <- out %>% mutate(estimate_quality = classify_quality(n_unweighted, estimate_cv))
      
      if (!isTRUE(input$show_n)) out <- out %>% select(-any_of("n_unweighted"))
      if (!isTRUE(input$prec_cv)) out <- out %>% select(-any_of("estimate_cv"))
      
      out <- out %>% rename(estimate_prop = estimate)
      
      return(out)
    }
    
    ## Numeric estimators ----
    var <- if (length(input$variable) > 0 && !is.null(input$variable) &&
               input$variable[1] != "") input$variable[1] else NULL
    
    validate(need(!is.null(var), "Select an estimate variable."))
    
    validate(need(
      !is_labelled(var),
      "This estimate type needs a numeric variable. The variable you picked is categorical."
    ))
    
    ## Second variable (Ratio and Correlation only) ----
    var2 <- NULL
    
    if (type %in% two_var_types) {
      var2 <- if (length(input$variable2) > 0 && !is.null(input$variable2) &&
                  input$variable2[1] != "") input$variable2[1] else NULL
      
      validate(need(
        !is.null(var2),
        if (type == "ratio") "Select a denominator variable." else "Select a second variable (Y)."
      ))
      
      validate(need(
        !is_labelled(var2),
        "This estimate type needs a numeric variable. The second variable you picked is categorical."
      ))
    }
    
    if (length(gbys) > 0) d <- d %>% group_by(across(all_of(gbys)))
    
    ## Quantile: 3 quality columns ----
    if (type == "quantile") {
      
      out <- d %>%
        summarise(
          estimate = survey_quantile(
            !!sym(var), quantiles = c(0.25, 0.5, 0.75),
            vartype = vt_calc, na.rm = TRUE
          ),
          n_unweighted = unweighted(sum(!is.na(!!sym(var))))
        )
      
      out <- out %>%
        mutate(
          estimate_quality_q25 = classify_quality(n_unweighted, estimate_q25_cv),
          estimate_quality_q50 = classify_quality(n_unweighted, estimate_q50_cv),
          estimate_quality_q75 = classify_quality(n_unweighted, estimate_q75_cv)
        )
      
      if (!isTRUE(input$show_n)) out <- out %>% select(-any_of("n_unweighted"))
      if (!isTRUE(input$prec_cv)) {
        out <- out %>%
          select(-any_of(c("estimate_q25_cv", "estimate_q50_cv", "estimate_q75_cv")))
      }
      
      return(out)
    }
    
    ## Mean / Total / Median / Variance / Std deviation / Ratio / Correlation ----
    ## Build the column names once. The second one stays NULL unless needed,
    ## so the unused branches below never try to convert NULL to a symbol.
    var_sym  <- sym(var)
    var2_sym <- if (!is.null(var2)) sym(var2) else NULL
    
    out <- d %>%
      summarise(
        estimate = switch(
          type,
          "mean"   = survey_mean(!!var_sym, vartype = vt_calc, na.rm = TRUE),
          "total"  = survey_total(!!var_sym, vartype = vt_calc, na.rm = TRUE),
          "median" = survey_median(!!var_sym, vartype = vt_calc, na.rm = TRUE),
          "var"    = survey_var(!!var_sym, vartype = vt_se_ci, na.rm = TRUE),
          "sd"     = survey_sd(!!var_sym, na.rm = TRUE),
          "ratio"  = survey_ratio(!!var_sym, !!var2_sym, vartype = vt_calc, na.rm = TRUE),
          "corr"   = survey_corr(!!var_sym, !!var2_sym, vartype = vt_se_ci, na.rm = TRUE)
        ),
        n_unweighted = if (type %in% two_var_types) {
          unweighted(sum(!is.na(!!var_sym) & !is.na(!!var2_sym)))
        } else {
          unweighted(sum(!is.na(!!var_sym)))
        }
      )
    
    ## Variance, std deviation and correlation: no quality rating ----
    if (type %in% no_quality_types) {
      out <- out %>% mutate(estimate_quality = NA_character_)
    } else {
      out <- out %>% mutate(estimate_quality = classify_quality(n_unweighted, estimate_cv))
    }
    
    ## Variance and correlation: drop the standard error if it was not ticked
    if (type %in% c("var", "corr") && !isTRUE(input$prec_se)) {
      out <- out %>% select(-any_of("estimate_se"))
    }
    
    if (!isTRUE(input$show_n)) out <- out %>% select(-any_of("n_unweighted"))
    if (!isTRUE(input$prec_cv)) out <- out %>% select(-any_of("estimate_cv"))
    
    rename_map <- c(
      mean = "estimate_mean", total = "estimate_tot", median = "estimate_median",
      var = "estimate_var", sd = "estimate_sd",
      ratio = "estimate_ratio", corr = "estimate_corr"
    )
    
    if (type %in% names(rename_map)) {
      out <- out %>% rename(!!rename_map[[type]] := estimate)
    }
    
    out
  })
  
  ## Add trailing "*" to quality column headers for display ----
  display_table <- reactive({
    df <- table_data()
    names(df) <- ifelse(
      grepl("^estimate_quality", names(df)),
      paste0(names(df), "*"),
      names(df)
    )
    df
  })
  
  output$result <- renderTable({
    display_table()
  }, digits = 4)
  
  ## Legend below the table: only when a quality column is present ----
  ## Errors are swallowed here so the message appears only once (in the results table)
  output$quality_legend_ui <- renderUI({
    df <- tryCatch(table_data(), error = function(e) NULL)
    if (is.null(df) || !has_quality_col(df)) return(NULL)
    
    legend_lines <- quality_legend_lines
    if (input$esttype %in% no_quality_types) {
      legend_lines <- c(legend_lines, no_quality_note_line)
    }
    
    tags$div(
      style = "font-size: 12px; color: #444; margin-top: 8px;",
      lapply(legend_lines, function(l) tags$div(l))
    )
  })
  
  ## Download: data + legend appended at the bottom ----
  output$export <- downloadHandler(
    
    filename = function() paste0("cius_estimate_", Sys.Date(), ".csv"),
    
    content = function(file) {
      df <- table_data()
      
      write.csv(df, file, row.names = FALSE)
      
      if (has_quality_col(df)) {
        legend_lines <- quality_legend_lines
        if (input$esttype %in% no_quality_types) {
          legend_lines <- c(legend_lines, no_quality_note_line)
        }
        
        cat("\n", file = file, append = TRUE)
        cat(paste(legend_lines, collapse = "\n"), file = file, append = TRUE)
        cat("\n", file = file, append = TRUE)
      }
    }
  )
}

shinyApp(ui, server)
