# Run one scenario of the JG model in the normal macroeconomic state, check its
# accounting and write the results.
# Usage: Rscript --vanilla run_example.R N0 [output_directory]
# N0 is the no-policy baseline; N1-N6 are the six policy designs (see README).

main <- function() {
  # Windows may start R in the C locale even when paths contain Unicode.
  # Change character encoding only; retain numerical locale and model equations.
  old_ctype <- Sys.getlocale("LC_CTYPE")
  if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
    if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
      stop("A UTF-8 Windows character locale is required for Unicode paths.", call. = FALSE)
    }
    on.exit(Sys.setlocale("LC_CTYPE", old_ctype), add = TRUE)
  }
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) != 1L) stop("Run this file with Rscript.", call. = FALSE)
  code_dir <- dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
  args <- commandArgs(TRUE)
  scenario <- if (length(args)) args[[1L]] else "N0"
  if (length(args) > 2L || !grepl("^N[0-6]$", scenario)) {
    stop("Usage: Rscript --vanilla run_example.R N0 [output_directory]; N0-N6 only.",
         call. = FALSE)
  }
  if (!requireNamespace("ineq", quietly = TRUE)) {
    stop("Missing R package 'ineq'. Install it explicitly before running.", call. = FALSE)
  }
  output_dir <- if (length(args) == 2L) args[[2L]] else
    file.path(code_dir, "..", "outputs", scenario)
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Output directory is not empty; choose a new directory.", call. = FALSE)
  }
  if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE)) {
    stop("Cannot create output directory.", call. = FALSE)
  }
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  log_file <- file.path(output_dir, "run.log")
  writeLines(c(paste("START", scenario), R.version.string,
               paste("ineq", packageVersion("ineq")),
               "Normal state; 100 periods; 10 original paths; 100 fixed sweeps per period.",
               "Source resets the RNG using set.seed(j) inside its loops; retained unchanged."),
             log_file)
  old_options <- options(jg.run_diagnostics = FALSE, jg.policy_scale_mode = "prepolicy_gdp")
  on.exit(options(old_options), add = TRUE)
  tryCatch({
    model <- new.env(parent = globalenv())
    model$shock <- as.integer(sub("N", "", scenario))
    model$phase3_spec <- list(policy = model$shock, shock_type = "none", n_periods = 100L)
    message("Running model scenario: ", scenario)
    started <- proc.time()[["elapsed"]]
    sys.source(file.path(code_dir, "equation_ap_csv1.R"), envir = model)
    analysis <- new.env(parent = globalenv())
    sys.source(file.path(code_dir, "model_analysis.R"), envir = analysis)

    periods <- 2:100
    rows <- list()
    add_check <- function(name, values, tolerance = 1e-8) {
      finite <- all(is.finite(values))
      gap <- if (finite) max(abs(values)) else Inf
      rows[[length(rows) + 1L]] <<- data.frame(
        check = name, max_abs_gap = gap, tolerance = tolerance,
        status = if (finite && gap <= tolerance) "PASS" else "FAIL")
    }
    core_names <- c("y", "c", "id", "p", "un", "ydl", "ydlm", "ydm", "ydum", "ydu")
    finite_core <- all(vapply(core_names, function(n)
      all(is.finite(model[[n]][, periods, drop = FALSE])), logical(1)))
    add_check("selected_core_values_finite", if (finite_core) 0 else Inf, 0)
    add_check("prices_positive", if (all(model$p[, periods] > 0)) 0 else Inf, 0)
    add_check("output_positive", if (all(model$y[, periods] > 0)) 0 else Inf, 0)
    for (n in names(analysis$phase3_sfc_residuals(model))) {
      add_check(n, analysis$phase3_sfc_residuals(model)[[n]][, periods, drop = FALSE])
    }
    add_check("output_identity", (model$y-model$c-model$id-model$gov-model$tb-model$hosn)[,periods])
    add_check("government_debt_update", model$bs[,periods]-model$bs[,periods-1L]-model$def[,periods])
    add_check("household_income_aggregation",
              (model$yd-model$ydl-model$ydlm-model$ydm-model$ydum-model$ydu)[,periods])
    add_check("household_consumption_aggregation",
              (model$c-model$cl-model$clm-model$cm-model$cum-model$cu)[,periods])
    add_check("net_cost_target", (model$phase3_policy_net_fiscal_cost-
                                  model$phase3_common_net_fiscal_cost_target)[,periods])

    sources <- analysis$inc_sources_grouped(model)
    targets <- analysis$yd_of(model)
    for (g in analysis$GROUPS) {
      add_check(paste0("income_sources_",g),
                (Reduce(`+`,sources[[g]])-targets[[g]])[,56:100])
    }
    contributions <- list()
    for (path in 3:10) {
      cell <- lapply(sources, function(group) vapply(group, function(x) x[path,100L], numeric(1)))
      total <- vapply(cell, sum, numeric(1))
      ly <- analysis$ly_contrib(cell, analysis$FAC_INC)
      cv2 <- analysis$cv2_contrib(cell, analysis$FAC_INC)
      add_check(paste0("income_gini_reconciliation_path_",path),
                sum(ly)-ineq::ineq(total,type="Gini"))
      add_check(paste0("income_cv2_reconciliation_path_",path),
                sum(cv2)-mean((total-mean(total))^2)/mean(total)^2)
      contributions[[length(contributions)+1L]] <- data.frame(
        scenario=scenario,path=path,period=100L,channel=names(ly),
        gini_contribution=unname(ly),cv2_contribution=unname(cv2))
    }
    checks <- do.call(rbind,rows)
    write.csv(checks,file.path(output_dir,"checks.csv"),row.names=FALSE)
    if (any(checks$status != "PASS")) stop("Scoped checks failed; inspect checks.csv.")

    grid <- expand.grid(path=1:10,period=56:100)
    result <- cbind(data.frame(scenario=scenario),grid)
    variables <- c(core_names,"gov","tb","hosn","nd","ng","ns","gini",
                   "phase3_policy_net_fiscal_cost","phase3_common_net_fiscal_cost_target")
    for (n in variables) result[[n]] <- model[[n]][as.matrix(grid)]
    write.csv(result,file.path(output_dir,"model_series.csv"),row.names=FALSE)
    write.csv(do.call(rbind,contributions),file.path(output_dir,"income_source_contributions.csv"),
              row.names=FALSE)
    writeLines(capture.output(sessionInfo()),file.path(output_dir,"session_info.txt"))
    elapsed <- proc.time()[["elapsed"]]-started
    cat(sprintf("SUCCESS %s; %d scoped checks passed; %.2f seconds\n",
                scenario,nrow(checks),elapsed),file=log_file,append=TRUE)
    message(sprintf("SUCCESS %s: %d scoped checks; %.2f seconds. Output: %s",
                    scenario,nrow(checks),elapsed,output_dir))
    invisible(model)
  },error=function(e) {
    cat(paste("FAILED",conditionMessage(e),"\n"),file=log_file,append=TRUE)
    stop(e)
  })
}

main()
