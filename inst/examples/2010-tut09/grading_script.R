library(autoharp)
library(progress)
library(dplyr)

soln_template_dir <- system.file('examples', '2010-tut09', 'soln',  
                                 package='autoharp')
demo_files_dir <- system.file('examples', '2010-tut09', 'demo_scripts',  
                              package='autoharp')
output_dir <- file.path(getwd(), "test_output")

s_env <- populate_soln_env(soln_fname = file.path(soln_template_dir,  
						  "tutorial_09_soln_template.qmd"),
                           pattern="test",
                           knit_root_dir = soln_template_dir)
fnames <- list.files(demo_files_dir, full.names = TRUE)

corr_list <- vector(mod="list", length=length(fnames))
pb <- progress_bar$new(total=length(fnames))
for(i in 1:length(fnames)){
  corr_list[[i]] <- render_one(fnames[i], out_dir=output_dir,
                    knit_root_dir = demo_files_dir, 
                    soln_stuff = s_env,
                    permission_to_install = TRUE,
                    max_time_per_run = 120)
  pb$tick()
}

corr_df <- dplyr::bind_rows(corr_list)
# unlink("test_out", TRUE)
