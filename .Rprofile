# nolint start
source("renv/activate.R")

if (Sys.getenv("CI") == "") { # not CI

  a <- NULL
  suppressMessages(if (requireNamespace("languageserver", quietly = TRUE)) {
    a <- try(suppressWarnings(source(file.path(
      Sys.getenv(if (.Platform$OS.type == "windows") {
        "USERPROFILE"
      } else {
        "HOME"
      }),
      ".vscode-R",
      "init.R"
    ))),
    silent = TRUE
    )
  })

  if (class(a) == "try-error") {
    message("Starting Binder Session")
    setHook("rstudio.sessionInit", function(newSession) {
      if (newSession & is.null(rstudioapi::getActiveProject())) {
        rstudioapi::openProject("false.alarm.Rproj")
      }
    }, action = "append")
  }

  rm(a)

  if (interactive()) {
    options(
      warnPartialMatchArgs = FALSE,
      warnPartialMatchDollar = FALSE,
      warnPartialMatchAttr = FALSE,
      usethis.protocol = "https",
      vsc.rstudioapi = TRUE
      # error = recover
    )

    suppressMessages(
      suppressWarnings({
        require("testthat", quietly = TRUE)
        require("devtools", quietly = TRUE)
        require("usethis", quietly = TRUE)
        require("conflicted", quietly = TRUE)
        require("here", quietly = TRUE)
        require("workflowr", quietly = TRUE)
        require("targets", quietly = TRUE)
        require("tarchetypes", quietly = TRUE)
      })
    )
    # suppressMessages(prettycode::prettycode())

    if (suppressMessages(requireNamespace("prompt", quietly = TRUE))) {
      prompt::set_prompt(function(...) {
        paste0(
          "[",
          prompt::git_branch(),
          prompt::git_dirty(),
          prompt::git_arrows(),
          "] ",
          prompt::prompt_runtime()
        )
      })
    }
  }
} else { # is CI
  suppressMessages(
    suppressWarnings({
      require("here", quietly = TRUE)
      require("workflowr", quietly = TRUE)
      require("targets", quietly = TRUE)
      require("tarchetypes", quietly = TRUE)
    })
  )
}



# nolint end
