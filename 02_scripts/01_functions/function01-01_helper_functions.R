## --------------------------------------------------------------#
## Script name: function01-01_helper_functions.R
##
## Purpose of script:
##    Provide reusable helper functions for workflow management
##    and common operations across analysis scripts.
##
## Dependencies:
##    - None (standalone function library)
##
## Author: Paul Bzonek
##
## Date Created: 2024-11-27
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#


#-------------------------------------------------------------#
#####func_source_clean ####################################----
#-------------------------------------------------------------#
#' Source R scripts with verbosity control
#'
#' @description
#' A wrapper around `source()` that provides flexible control over console output
#' verbosity. Useful for cleaning up console output when sourcing multiple scripts
#' in a workflow, while maintaining the ability to debug individual scripts when needed.
#'
#' @param file Character string specifying the path to the R script file to source.
#'   Accepts both relative and absolute paths.
#' @param level Character string controlling verbosity level. Must be one of:
#'   \itemize{
#'     \item \code{"debug"} - Shows code expressions, output, and progress messages
#'       (most verbose, for troubleshooting)
#'     \item \code{"full"} - Shows output and progress messages but not code expressions
#'       (default interactive mode)
#'     \item \code{"minimal"} - Shows cat() output and errors, suppresses warnings,
#'       messages like "Joining with...", and plots (recommended for production runs)
#'     \item \code{"silent"} - Completely silent execution with no console output
#'       (for automated pipelines)
#'     \item \code{"code_only"} - Shows code expressions but suppresses output
#'       (rare debugging use case)
#'   }
#'   Defaults to \code{"minimal"}.
#' @param beep_on_complete Logical; if TRUE, plays a sound after script completes (requires beepr package).
#'   Useful for marking the end of a selected code block. Defaults to FALSE.
#' @param beep_on_error Logical; if TRUE, plays an alarm sound when an error occurs (requires beepr package).
#'   Useful for audio notification of script failures. Defaults to FALSE.
#'
#' @return
#' Invisibly returns the result of `source()`. Side effects include loading
#' objects, functions, and variables from the sourced script into the current
#' environment, and optionally printing progress/output to console based on
#' verbosity level.
#'
#' @details
#' The function uses `switch()` to control both the `echo` argument of `source()`
#' (which controls code visibility) and `capture.output()` (which suppresses
#' output like `cat()`, `print()`, and plot displays).
#'
#' Progress messages show only the basename of the file for cleaner output.
#'
#' @examples
#' # Minimal output (just progress)
#' func_source_clean("02 - Scripts/Script1-1_format_data.R", level = "minimal")
#'
#' # Full debugging with code and output
#' func_source_clean("02 - Scripts/Script1-1_format_data.R", level = "debug")
#'
#' # Completely silent
#' func_source_clean("02 - Scripts/Script1-1_format_data.R", level = "silent")
#'
#' # Using with a global parameter
#' param_verbose <- "minimal"
#' func_source_clean("02 - Scripts/Script1-1_format_data.R", level = param_verbose)
#'
#' @export
func_source_clean <- function(file, level = "minimal", beep_on_complete = FALSE, beep_on_error = FALSE) {

  #----------------------------
  # 1. Validate inputs
  #----------------------------
  valid_levels <- c("debug", "full", "minimal", "silent", "code_only")
  if (!level %in% valid_levels) {
    stop("`level` must be one of: ", paste(valid_levels, collapse = ", "))
  }

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  if ((beep_on_complete || beep_on_error) && !requireNamespace("beepr", quietly = TRUE)) {
    warning("beepr package not installed. Beep will not sound.")
    beep_on_complete <- FALSE
    beep_on_error <- TRUE
  }

  #----------------------------
  # 2. Execute source with verbosity control and error handling
  #----------------------------
  tryCatch(
    {
       switch(level,

              "debug" = {
                # Shows code + output + progress message
                cat("\n=== SOURCING:", basename(file), "===\n")
                source(file, echo = TRUE)
                cat("=== COMPLETE ===\n\n")
              },

              "full" = {
                # Shows output but NOT code, with progress message
                cat("Loading:", basename(file), "...")
                source(file, echo = FALSE)
                cat(" Done.\n")
              },

              "minimal" = {
                # Shows cat output and errors, suppresses warnings and messages
                cat("\n---", basename(file), "---\n")
                suppressMessages(suppressWarnings(source(file, echo = FALSE)))
                cat("--- COMPLETE ---\n")
              },

              "silent" = {
                # Completely silent - no progress, no output, no code
                cat("\n---", basename(file), "---\n")
                pdf(NULL)  # Redirect plots to null device
                suppressMessages(suppressWarnings(
                  invisible(capture.output(source(file, echo = FALSE)))
                ))
                dev.off()  # Close null device
              },

              "code_only" = {
                # Shows code but suppresses output (rare use case)
                cat("\n>>> Code from:", basename(file), "\n")
                invisible(capture.output(source(file, echo = TRUE)))
              }
            ) #End switch
    }, #End main try catch
    error = function(e) {
      # Play error beep if requested
      if (beep_on_error) {
        beepr::beep(sound = 7)  # sound = 7 is alarm sound
     }
      # Re-throw the error to stop execution
      stop(e)
    }
  )

  #----------------------------
  # 3. Play completion sound if requested
  #----------------------------
  if (beep_on_complete) {
    beepr::beep(sound = 1)  # Completion beep
  }
}

#-------------------------------------------------------------#
#####fct_cite_packages_grateful ############################---
#-------------------------------------------------------------#
#' Generate BibTeX citations for R packages used in a project (via {grateful})
#'
#' @description
#' Creates a BibTeX bibliography of R package citations using the {grateful} package.
#' By default, it scans the full repository (recommended). Optionally, it can scan a
#' single script/report file (e.g., the load-packages script).
#'
#' Outputs default to the project outputs files folder (\code{03_outputs/02_files/}).
#'
#' @param scan Character string specifying scan mode. Must be one of:
#'   \itemize{
#'     \item \code{"repo"} - Scan the full repository (default; better default)
#'     \item \code{"file"} - Scan a single file specified by \code{file}
#'   }
#' @param file Character string path to a file to scan when \code{scan = "file"}.
#'   Defaults to the load-packages script in \code{02_scripts/}.
#' @param out_dir Character string output directory for BibTeX and report files.
#'   Defaults to \code{"03_outputs/02_files"}.
#' @param bib_file Character string name of the BibTeX output file.
#'   Defaults to \code{"packages.bib"}.
#' @param report_file Character string base name for the human-readable report file
#'   (extension added via \code{report_format}). Defaults to \code{"package-citations"}.
#' @param report_format Character string output format for the report. Defaults to \code{"md"}.
#'
#' @return Invisibly returns the path to the BibTeX file written.
#'
#' @details
#' Repo-wide scanning uses \code{pkgs = "All"} in \code{grateful::cite_packages()}.
#' Script-specific scanning uses \code{pkgs = <file>} and defaults to the project
#' load-packages script.
#'
#' @examples
#' # Repo-wide scan (recommended default)
#' # fct_cite_packages_grateful()
#'
#' # Scan only the load-packages script
#' # fct_cite_packages_grateful(scan = "file")
#'
#' # Scan any specific script/report
#' # fct_cite_packages_grateful(scan = "file", file = "02_scripts/scriptXX-XX_my_script.R")
#'
#' @export
fct_cite_packages_grateful <- function(
  scan = c("repo", "file"),
  file = "02_scripts/script00-01_load_packages.R",
  out_dir = "03_outputs/02_files",
  bib_file = "packages.bib",
  report_file = "package-citations",
  report_format = "md"
) {
 
 # 1) Validate arguments -------------------------------------------------------
 # match.arg() ensures scan is exactly "repo" or "file"
 # and sets the default to the first option ("repo").
 scan <- match.arg(scan)
 
 # 2) Check dependency --------------------------------------------------------
 # We require {grateful} because the actual citation generation happens there.
 # If it isn't installed, we stop with a clear message.
 if (!requireNamespace("grateful", quietly = TRUE)) {
  stop(
   "Package 'grateful' is required. Install it with install.packages('grateful').",
   call. = FALSE
  )
 }
 
 # 3) Define project root and output location ---------------------------------
 # We assume the working directory is the project root (typical RStudio project usage).
 root <- normalizePath(getwd(), mustWork = TRUE)
 
 # Build full output directory path and create it if it doesn't exist yet.
 # Example default: "<project_root>/03_outputs/02_files"
 out_dir_full <- file.path(root, out_dir)
 if (!dir.exists(out_dir_full)) {
  dir.create(out_dir_full, recursive = TRUE)
 }
 
 # 4) Decide what to scan for packages ----------------------------------------
 # "repo" mode:
 #   - pkgs = "All" tells grateful/renv to scan the whole repository recursively.
 #
 # "file" mode:
 #   - pkgs = <path_to_file> tells grateful to scan ONLY that one file.
 #   - default file points to the load-packages script.
 if (scan == "repo") {
  pkgs_arg <- "All"
 } else {
  file_full <- file.path(root, file)
  
  # If the file isn't found, fall back to repo-wide scanning so the function still works.
  if (!file.exists(file_full)) {
   warning("File not found: ", file, " — falling back to repo-wide scan.")
   pkgs_arg <- "All"
  } else {
   pkgs_arg <- normalizePath(file_full, mustWork = TRUE)
  }
 }
 
 # 5) Generate outputs --------------------------------------------------------
 # grateful writes TWO things when output = "file":
 #   (a) A BibTeX file (bib.file) -> used by Quarto/R Markdown as a bibliography
 #   (b) A readable report (out.file + out.format) -> quick human check
 #
 # Default outputs:
 #   03_outputs/02_files/packages.bib
 #   03_outputs/02_files/package-citations.md
 grateful::cite_packages(
  pkgs = pkgs_arg,
  output = "file",
  out.dir = out_dir_full,
  bib.file = bib_file,
  out.file = report_file,
  out.format = report_format,
  root = root
 )
 
 # 6) Return value -------------------------------------------------------------
 # Return the path to the BibTeX file (invisibly, so it doesn't spam console).
 invisible(file.path(out_dir_full, bib_file))
}
