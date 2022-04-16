r_tasks_jobs <- function(job_name) {
  setwd(here::here())

  switch(job_name,
    targets = {
      targets::tar_make_future(workers = 4L, reporter = "summary")
    },
    workflowr_build = {
      workflowr::wflow_build(update = TRUE)
    },
    thesis_down = {
      message("TODO")
    },
    test_coverage = {
      covr::package_coverage()
    },
    test_units = {
      testthat::test_local(reporter = testthat::SummaryReporter)
    },
    lint_project = {
      library(lintr)
      for (i in c("analysis", "R", "scripts", "review", "thesis", "output")) {
        print(lint_dir(path = i, linters = with_defaults(
          open_curly_linter = NULL,
          T_and_F_symbol_linter, assignment_linter,
          closed_curly_linter = NULL, commas_linter,
          commented_code_linter = NULL, cyclocomp_linter(50),
          object_name_linter(styles = "snake_case"),
          object_length_linter(30), equals_na_linter,
          function_left_parentheses_linter, infix_spaces_linter,
          line_length_linter(500), no_tab_linter, object_usage_linter = NULL,
          paren_brace_linter, absolute_path_linter(lax = TRUE),
          nonportable_path_linter = NULL, pipe_continuation_linter,
          semicolon_terminator_linter(semicolon = "trailing"), seq_linter,
          single_quotes_linter, spaces_inside_linter, spaces_left_parentheses_linter,
          trailing_blank_lines_linter, trailing_whitespace_linter, undesirable_function_linter,
          undesirable_operator_linter, unneeded_concatenation_linter
        ), exclusions = list("R/RcppExports.R")))
      }
    },
    style_project = {
      styler:::style_dir(
        filetype = c("R", "Rmd"),
        recursive = TRUE,
        exclude_files = "R/RcppExports.R",
        exclude_dirs = c("dev", "renv", ".history")
      )
    }
  )
}
