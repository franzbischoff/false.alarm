source(here::here(".vscode", "jobs.R"), encoding = "UTF-8")

expr <- "job_(.*?)\\.R"
arg_with_name <- grep(expr, commandArgs(), value = TRUE)
m <- gregexec(expr, arg_with_name)
y <- regmatches(arg_with_name, m)
job <- y[[1]][2]
r_tasks_jobs(job)
