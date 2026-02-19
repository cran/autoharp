## About this folder

This folder contains example worksheets that were used in previous courses. 
Each folder contains the worksheet, solution, solution template and data 
for that worksheet.

Each folder also contains a file `grading_script.R`, which can be used to run 
the autoharp for all "student" files in the `demo_scripts` folder. The paths in
this grading script will be set using `system.file()` calls. The only restriction
is that the working directory when you run the grading script should be a place that 
your account has permission to write to.

To run the grading script for `1910-tut04`, for instance, you just have to:

```
library(autoharp)
grading_script_path <- system.file(file.path('examples', '1910-tut04', 'grading_script.R'),
                                   package='autoharp')
source(grading_script_path, echo=TRUE)
```

Some of the demo scripts will fail; there are meant to demonstrate the sorts of 
situations in which the autoharp will bypass a script, or not be able to pick up 
a check.


## Description

1.  `1910-tut04`
    - Uses `sp` package
    - Students asked to write methods for an `S3` class.
    - Solution template demonstrates:
      - How to call the methods and test them on a vector.
      - How to compare if two vectors are equal
      - How to extract functions used from a particular package.
2.  `2010-tut02`
    - Uses `kjv` tokens
    - Worksheet includes plot creation.
    - Check user-defined Jaccard function on test-vectors, using object from
      model solution.
    - How to extract functions used from a particular package using forestharp.
3.  `2010-tut05`
    - Questions involve data manipulations with dplyr
    - Solution template checks for existence of datasets and their dimensions.
    - Solution template demonstrates the use of quarto.
4.  `2010-tut07`
    - Questions involve joins, data manipulations, data exploration
    - Solution template demonstrates how to check for loops, lambda functions, apply counts,
      and other checks.
5.  `2010-tut08`
    - Questions involve joins, data manipulations, data exploration
    - Solution template demonstrates the use of checking for geoms used in ggplot calls.
    - Good for use with `generate_thumbnails()` for class discussion.
6.  `2010-tut09`
    - A tutorial for students to explore data and generate plots.
    - Solution template demonstrates the use of checking for geoms used in ggplot calls.
    - Demonstrates use of `render_one` with `permission_to_install = TRUE`.
    - Good for use with `generate_thumbnails()` for class discussion.
7.  `2210-ass01`
    - Tutorial involving implementation of Akima interpolation.
    - Tests students use of vectorised operations within a function.
    - Solution template demonstrates use of extracting student-written functions 
      and executing them on test-cases.
8.  `2210-ass02`
    - Tutorial involves implementation of Box-Muller algorithm for generating
      standard Normal random variables.
    - Students also work on extractor function for a custom S3 object.
    - One tricky thing to note is that:
      - the function that students write takes a path as input.
      - When rendering the file, if students call their own function, relative 
        paths will work.
      - When correctness checks are run, relative paths will not work. Use an
        absolute path in the test chunks!
    - Solution template demonstrates the use of tryCatch, just in case student
      function fails.
    - Note: if a student function has an error, and it is executed in the student
      script, it cannot be rendered. Even if the test chunks wrap it in a tryCatch,
      the correctness checks will not be run. `render_one` will fail at the 
      rendering stage.
9.  `learnr-1910-tut04`
    - Demonstrates how tutorial/question sheets can also be distributed through 
      `learnr`.
    - Using learnr ensures paths are set correctly and that datasets are correctly 
      loaded.
    - Solution scripts downloaded from here can be used directly with the solution 
      template in `1910-tut04`.
10. `learnr-2010-tut05`
    - A second `learnr` example.
    - Demonstrates a case where, even if the student is unable to get the first 
      data frame, or if there is a syntax error in it, they can still proceed
      with the second exercise.
