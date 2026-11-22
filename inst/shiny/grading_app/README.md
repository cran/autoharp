# Autoharp Instructor Grading App

A Shiny application designed to help instructors grade student R/Rmd/QMD
scripts more easily when using the autoharp package.

The app follows one workflow across three tabs:

1. **Template Generation** – turn a plain model solution into an
   autoharp grading template with test chunks.
2. **Autoharp Grading** – grade all student submissions against the template
   with autoharp's `render_one()`.
3. **Plot Grading** – view the plots each student produced, give a plot mark
   and comments, and export the combined results.

Saving and loading sessions is available at any time from the buttons in the
top-right corner of the header.

---

## Session Management (header)

The **Save Session** and **Load Session** buttons sit in the top-right corner
of the app header. The status line underneath shows when progress was last saved,
for example `Progress auto-saved: session_20261102_120854.rds (12:08:54)`
(hover over it to see the full path).

### Auto-save

The session is saved automatically:

- after autoharp grading completes
- after each plot grade is saved
- when you move to another student after editing a mark or comment

Session files are written to `<output directory>/sessions/`.

### Save Session

Saves the current session immediately to `<output directory>/sessions/` and
shows where it was saved. If no output folder is known yet (no student scripts
folder has been set), the app offers to download the session file instead.

### Load Session

Opens a dialog where you upload a session file (`.rds`). The dialog confirms
the selected file and shows the **Current Session Info** (session ID, last
saved time and location, number of students and number graded). Loading restores:

- the model solution, its objects, the object selection and the generated template
- the grading template used in Autoharp Grading
- all folder settings, the output directory and the test chunk pattern
- the autoharp grading results and the list of student plots
- plot marks, comments and your current position in Plot Grading

The model solution is run again when a session is loaded, from the same
"run from" folder as before.

---

## Folder Paths

All folder fields (the student scripts folder in Autoharp Grading and the
"run from" folders) accept either an **absolute path** (e.g.
`/Users/me/course/hw1/sol`) or a **relative path**. Relative paths are resolved from the app's working directory, which is
the app folder when the app is started with `shiny::runApp()` (e.g. `test3/sol`
or `../hw1/sol`). The "Browse..." buttons fill in an absolute path.

As soon as you stop typing a student scripts folder, the app checks it:

- a pop-up message, as in earlier versions: **"Found N student files"**,
  **"No R, Rmd, or QMD files found in the specified folder"** or
  **"Folder does not exist"**
- a status line under the field showing the result (e.g.
  `Found 5 student files (3 .Rmd, 2 .R)`) and the full resolved path, so you can
  see exactly where a relative path points

For the "run from" folders the status line shows whether the folder exists.
When left empty, Template Generation shows that the model solution runs in a
temporary folder, and Autoharp Grading shows "Using the student scripts folder".

The output directory is different: a relative output directory is created next
to the student scripts folder (see [Where Files Are Saved](#where-files-are-saved)).

---

## Tab 1: Template Generation

This tab generates an autoharp-compatible grading template from a plain model
solution (no autoharp annotations needed).

**Setup (left sidebar)**

1. **Upload Model Solution (RMD/QMD)**: the instructor's solution file.
2. **Run Model Solution From (optional)**: the folder the model solution is
   run from. Set it whenever the solution reads data files: relative paths in
   the solution (e.g. `../data/file.csv`) are resolved from this folder. If the
   solution does not read any files, leave it empty and the solution runs in a
   temporary folder.
3. **Template Options**: the test chunk prefix (default `test`) and whether to
   include all comprehensive tests.
4. **Generate Template**: runs the model solution, identifies the objects it
   creates, and generates default tests based on their types.

The student scripts folder is not needed here: it is set in the Autoharp
Grading tab, where the grading happens.

If the model solution has errors (for example a data file that cannot be found),
the sidebar shows how many errors occurred and the first error messages,
including the name of any file that could not be opened. It also shows the
folder the solution was run from or, if no "run from" folder is set, suggests
setting one.

**Select Objects to Test (main panel)**

- Tick the objects to include in the grading template ("Select all" /
  "Select none" are available).
- The **Suggested Tests** table shows the type of each selected object and the
  tests that will be generated for it.
- **Update Template (optional)** refreshes the template preview after changing
  the selection or options. Downloads and "Use for Autoharp Grading" always use
  the current selection.
- **Download Template** saves the template as `<solution name>_autoharp.Rmd`
  (or `.qmd`).
- **Use for Autoharp Grading** passes the template, the "run from" folder and
  the test chunk prefix to the Autoharp Grading tab and switches to it.

The **Generated Template Preview** panel shows the full template, and the
**Generated Tests Documentation** panel lists the tests for each object type.

The "run from" folder and test chunk prefix are also copied to the Autoharp
Grading tab as you type them.

---

## Tab 2: Autoharp Grading

This tab grades all student submissions with autoharp's `populate_soln_env()`
and `render_one()`.

**Grading Setup (left sidebar)**

1. **Grading Template (RMD/QMD)**: upload a template, or use the one passed
   from Template Generation. The box under the upload shows which template is
   in use and where it came from.
2. **Path to Student Scripts Folder**: all `.R`, `.r`, `.Rmd`, `.rmd`, `.qmd`
   and `.Qmd` files in this folder are graded (absolute or relative path; the
   number of files found is shown as you type).
3. **Run Grading Template From (optional)**: the folder the grading template
   is run from (relative data paths in the template are resolved from here).
   If left empty, the student scripts folder is used. Student scripts always
   run from their own folder.
4. **Output Directory (optional)**: where rendered output, plots and session
   files are saved (default `student_out`). A relative path is created next to
   the student scripts folder; the full path is shown under the field.
5. **Test Chunk Pattern**: the pattern that identifies test chunks in the
   template (default `test`).
6. **Allow package installation during grading**: if ticked, the app installs
   any missing packages that the student scripts need.
7. **Continue past errors in student code** (ticked by default): see
   [Errors in Student Code](#errors-in-student-code).
8. **Run Automated Grading**, then **Export Results (Excel)**.

**Grading Results (main panel)**

- A status bar summarises how many students were processed, succeeded and failed.
- The results table shows each student's run status, run time, memory used and
  the result of every test (an Error column appears if any student failed).
  Once plots have been graded, the plot mark and comments are added to the table.
- If plots were found, a **Plots detected** bar shows how many students
  produced plots, with a button that opens Plot Grading.

---

### Errors in Student Code

With **Continue past errors in student code** ticked (the default), a student's
script keeps running after an error, so the remaining objects are still created
and tested. This is how the original Object Testing grading behaved. For example,
a stray line such as `S` (`object 'S' not found`) no longer stops the rest of
the script.

- The student is marked **SUCCESS**, and the error is listed in a
  **Code Errors** column (e.g. `1: object 'S' not found`), in the student's
  Test Results summary in Plot Grading, and in the Excel export
  (`code_errors`, `code_error_messages`).
- The status bar shows how many students had errors in their code.
- Line numbers in error messages may be about 3 lines higher than in the
  student's file because of the added chunk. Template checks that analyse the
  student's source code (rather than the objects it creates) will also see this
  one extra line.

With the box unticked, a script stops at its first error and is marked
**FAIL** with no test results, as in the original Autoharp Grading tab.

Scripts that cannot run at all are still marked **FAIL**: syntax errors that
stop the file from being parsed, scripts that exceed the time limit, missing
packages (when package installation is not allowed) and scripts that call
`system()`.

---

## Tab 3: Plot Grading

**Left sidebar**

- **Student Selection**: choose a student from the list, or use Previous / Next.
- **Plot Grading**: enter a plot mark and comments, then click
  **Save Plot Grade**. The app saves the grade, auto-saves the session and moves
  to the next student. If you edit a mark or comment and move to another
  student without saving, the change is kept.
- **Progress**: how many students have been graded.

**Main panel**

- **Student Plot**: one plot at a time, with "Plot k of n" arrows to move
  between a student's plots, a Plot Information panel (file name, size,
  creation time) and a full-size view.
- **Test Results for Selected Student**: the selected student's run status, run
  time and memory, and every autoharp test with its result (PASS / FAIL) and value.
- **Consolidated Grading Results**: autoharp test results (PASS / FAIL), plot
  marks, comments and whether each student has been graded, with
  **Export Results (Excel)**.

Plots are collected from the autoharp output folder
(`<output directory>/<student>_files/figure-html/`). This works for `.R`
scripts as well as Rmd/QMD files, including ggplot plots.

### Excel export

The Excel file (available in both Autoharp Grading and Plot Grading) has two sheets:

- **Grading Results**: run status, run time, memory, all test results, any
  error message, the errors in the student's code, and the plot mark and
  comments for each student
- **Plot Grades**: plot marks, comments and whether each student has been graded

---

## Where Files Are Saved

```
<parent of student scripts folder>/
├── <student scripts folder>/        # student submissions (unchanged)
└── student_out/                     # output directory (configurable)
    ├── sessions/                    # session files (.rds)
    │   └── session_20261102_120854.rds
    ├── <student>.html               # rendered output from render_one
    ├── <student>_files/figure-html/ # plots produced by each student
    └── render_one.log               # log file from render_one
```

Session files store the paths to the plot files. If the output directory is
moved or deleted, Plot Grading shows a "Plot file not found" message; re-run
the grading to recreate the plots.

---

## Default Tests by Object Type

Template Generation creates these tests for each selected object. Tests marked
"comprehensive" are only added when "Include all comprehensive tests" is ticked.

### Numeric Vectors

| Test | Description | Comprehensive |
|------|-------------|---------------|
| `length` | Compares length | |
| `mean` | Compares mean (tolerance 1e-6) | |
| `identical` | Checks all values match (tolerance 1e-6) | |
| `sd` | Compares standard deviation | yes |
| `min`, `max` | Compares minimum and maximum | yes |
| `sum` | Compares sum | yes |
| `na_count` | Compares number of NA values | yes |

### Character Vectors

| Test | Description | Comprehensive |
|------|-------------|---------------|
| `length` | Compares length | |
| `identical` | Checks all values match | |
| `unique_count` | Compares number of unique values | yes |
| `nchar_total` | Compares total number of characters | yes |

### Logical Vectors

| Test | Description | Comprehensive |
|------|-------------|---------------|
| `length` | Compares length | |
| `identical` | Checks all values match | |
| `sum_true` | Compares number of TRUE values | |
| `na_count` | Compares number of NA values | yes |

### Data Frames

| Test | Description | Comprehensive |
|------|-------------|---------------|
| `nrow`, `ncol` | Compares dimensions | |
| `colnames` | Checks column names match | |
| `coltypes` | Checks column types match | yes |
| `col_<name>_mean` | Compares means of numeric columns (up to 3) | yes |

### Matrices

| Test | Description | Comprehensive |
|------|-------------|---------------|
| `nrow`, `ncol`, `dim` | Compares dimensions | |
| `rownames`, `colnames` | Checks names match | yes |
| `mean` | Compares overall mean | yes |
| `max_diff` | Checks largest element difference | yes |

### Lists

| Test | Description | Comprehensive |
|------|-------------|---------------|
| `length` | Compares number of elements | |
| `names` | Checks element names match | |
| `elem_<name>` | Compares named elements (up to 3) | yes |

### Functions

| Test | Description | Comprehensive |
|------|-------------|---------------|
| `num_args` | Compares number of arguments | |
| `arg_names` | Checks argument names match | |
| `is_function` | Checks the object is a function | yes |

### Factors

| Test | Description | Comprehensive |
|------|-------------|---------------|
| `length` | Compares length | |
| `nlevels`, `levels` | Compares the levels | |
| `level_order` | Checks level order matches | yes |
| `frequencies` | Compares level frequencies | yes |

### Other Objects

| Test | Description | Comprehensive |
|------|-------------|---------------|
| `class` | Checks classes match | |
| `identical` | Checks objects are equal | |

---


## How to Run

From an R session with autoharp installed:

```r
shiny::runApp(system.file("shiny/grading_app", package = "autoharp"))
```


## File Structure

```
grading_app/
├── app.R          # Main Shiny application (UI and server)
├── R/helpers.R    # Test generation and plot collection helpers
├── www/styles.css # App styling
└── README.md      # This file
```

## Dependencies 

- shiny, DT, knitr, rmarkdown, base64enc
- autoharp (for `populate_soln_env()` and `render_one()`)
- shinyFiles (for the "Browse..." folder selectors)
- openxlsx (optional, for Excel export; the app prompts to install it if missing)
