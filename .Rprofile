# nolint start
if (.Platform$OS.type == "windows") {
  Sys.setenv(LC_CTYPE = "C")
}

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
    ) # if this fails we (probably) are in Binder
  })

  if (class(a) == "try-error") { # we are in Binder session (hopefully)
    message("Starting Binder Session")
    setHook("rstudio.sessionInit", function(newSession) {
      if (newSession & is.null(rstudioapi::getActiveProject())) {
        rstudioapi::openProject("false.alarm.Rproj")
      }
    }, action = "append")
  }

  rm(a)

  if (interactive() && Sys.getenv("RSTUDIO") == "") {
    options(
      warnPartialMatchArgs = FALSE,
      warnPartialMatchDollar = FALSE,
      warnPartialMatchAttr = FALSE,
      usethis.protocol = "https",
      warn = 1 # warnings appear immediately, not in the end
      # error = recover
    )
    options(
      vsc.rstudioapi = TRUE,
      # vsc.browser = "Two",
      # vsc.viewer = "Two",
      # vsc.page_viewer = "Two",
      # vsc.view = "Two",
      # vsc.plot = "Two",
      # vsc.helpPanel = "Two",
      vsc.str.max.level = 2,
      vsc.show_object_size = TRUE,
      vsc.globalenv = TRUE,
      vsc.dev.args = list(width = 1000, height = 300)
    )

    # if httpgd is installed, let's use it
    # This breaks rendering video
    # if ("httpgd" %in% .packages(all.available = TRUE)) {
    #   options(vsc.plot = FALSE)
    #   options(device = function(...) {
    #     httpgd::hgd(silent = TRUE)
    #     .vsc.browser(httpgd::hgd_url(history = FALSE), viewer = "Beside")
    #   })
    # }

    suppressMessages(
      suppressWarnings({
        require("testthat", quietly = TRUE)
        require("devtools", quietly = TRUE)
        require("usethis", quietly = TRUE)
        require("conflicted", quietly = TRUE)
        require("here", quietly = TRUE)
        require("workflowr", quietly = TRUE)
        require("targets", quietly = TRUE)
        require("gittargets", quietly = TRUE)
        require("tarchetypes", quietly = TRUE)
      })
    )

    if (suppressMessages(requireNamespace("prettycode", quietly = TRUE))) {
      suppressMessages(prettycode::prettycode())
    }

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

    if (Sys.getenv("RADIAN_VERSION") == "") {
      loadhistory() # if no file, no problem.

      # Cleaning up function
      .Last <- function() {
        savehistory() # comment this line if you don't want to save history
        cat("bye bye...\n") # print this so we see if any non-interactive session is lost here
      }
    }
  } else { # is RSTUDIO
    suppressMessages(
      suppressWarnings({
        require("here", quietly = TRUE)
        require("workflowr", quietly = TRUE)
        require("targets", quietly = TRUE)
        require("tarchetypes", quietly = TRUE)
        require("gittargets", quietly = TRUE)
      })
    )
  }
} else { # is CI
  suppressMessages(
    suppressWarnings({
      require("here", quietly = TRUE)
      require("workflowr", quietly = TRUE)
      require("targets", quietly = TRUE)
      require("tarchetypes", quietly = TRUE)
      require("gittargets", quietly = TRUE)
    })
  )
}



# nolint end
