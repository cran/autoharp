# autoharp 0.3.2
* fixed critical errors in vignettes

# autoharp 0.3.1
* added 5 vignettes

# autoharp 0.3.0
* moved grading app to inst/shiny folder

# autoharp 0.2.1

* Added fix to `wrap_chunks` to work with qmd test chunks correctly.
* Reduced package size by using bzip2 compression for `retail_clean.rds`.
* Grading up path usage and plot presentation fixed.

# autoharp 0.2.0

* `render_prechecks` updated to only look for `system` and `system2`.
* `rmd_to_forestharp` always returns a list of length 2.
  * Uses `purl` to detect chunks better.
  * lintr lines matched with purl lines one at a time.
* `subtree_at()` made more robust by improving `get_recursive_index()`.
* `extract_actual_args` allows ignoring of assigned objects.
* Added ForestHarp helpers to count for loops, detect possible improvements with
  pipe or apply.

# autoharp 0.1.1

* Replaced `remove_extension` with `xfun::sans_ext()`.
* Added pre-checks (for `view()` and `system()`) for `render_one`.
* `extract_chunks()` fixed to handle verbatim chunks in Rmd files.
* Bug in `count_fn_call` fixed: when magrittr pipe is used, it no longer
  classifies the piped object as not-a-call.

# autoharp 0.1.0

* Tuner Shiny application moved to `inst/shiny`.
* Works with `shinymanager` for password protection.

# autoharp 0.0.13

* Removed dependency on `pryr`.
* Updated to work with new version of `lintr`.

# autoharp 0.0.12

* Fixed missing link.
* Replaced `class(x) == "string"` with use of `is()` function.

# autoharp 0.0.11

* Removed `examplify_to_r`.
* Added `extract_section_text` function.

# autoharp 0.0.10

* Bug in `log_summary` fixed.
* Error messages no longer use `object$message`; now use `conditionMessage(object)`
  because some dplyr functions return conditions, not error objects.

# autoharp 0.0.9

* Position argument warning in `run_tuner()` handled (argument deprecated in Bootstrap).
* Added URL to package documentation in DESCRIPTION.
* Bug in `render_one` fixed (refers to `try_out`, which is not present anymore),
  when a library could not be installed.
* `extract_chunks` in `get_libraries` and `rmd_to_forestharp` is now wrapped in
  a `tryCatch` call.
* `extract_non_chunks` now works if there are no chunks — returns all lines.

# autoharp 0.0.8

* Vignettes moved out of package to GitHub Pages.
* Temp R script trimmed to exclude assign calls, since we are no longer running
  it under `test_file` but using `source()` instead.

# autoharp 0.0.7

* `purl` set to `FALSE` to prevent R file from being generated.
* `shiny` not imported entirely.

# autoharp 0.0.6

* Allow `populate_soln_env` to specify where to write the temp knitted output.
* Ensure that `user-manual.Rmd` only writes to `tempdir()`.
* `testthat` moved to Suggests.

# autoharp 0.0.5

* Check for pandoc availability before building vignettes or running tests.

# autoharp 0.0.4

* `check_correctness` also runs in a separate process.
* Lint counter function added (`count_lints`).
* `run_tuner` asks for permission before installation.
* `generate_thumbnails` uses anonymous filenames.

# autoharp 0.0.3

* `render_one` runs the student script in a separate process, avoiding the need
  to check the user workspace or reset search paths.
* testthat chunks are no longer used in solution templates. This prevents problems
  if the testthat format changes.

# autoharp 0.0.2

* `render_one` allows skipping correctness checks by setting `soln_stuff` to `NA`.

# autoharp 0.0.1.2

* `check_rmd` now just checks for chunks, uses `extract_chunks` directly.
* ForestHarp helper to detect nested for loops added.
* Bug fix for `lang_2_tree`, when a function definition had `NULL` as a default argument.

# autoharp 0.0.1.01

* Fixed issue where `render_one()` would fail if the solution template did not
  generate all the declared scalars. The check now happens during `populate_soln_env()`.

# autoharp 0.0.0.9008

* Tuner app now takes in a list of lints; output dataframe can be controlled by
  specifying columns to drop.

# autoharp 0.0.0.9007

* TreeHarp constructors updated to work with output of `class()` from R v4.0.0.

# autoharp 0.0.0.9005

* testthat output handles tests such as `setequal` (which actually runs two tests).
* Max run time added to `render_one`.
* More tests added now that non-test-named files can be placed in `tests/`.

# autoharp 0.0.0.9004

* testthat output parsed better.
* Scalars must be atomic; they are also checked against template objects.
* More solution templates added.

# autoharp 0.0.0.9003

* Hooks renamed to `autoharp.objs` and `autoharp.scalars`.
* Solution objects, scalars to keep, and filename are stored as hidden objects in
  the environments.
* Both testthat chunks and static code chunks can be in the same solution template.
