# inspired by Nicola Rennie.
# https://nrennie.rbind.io/blog/script-templates-r/

library(here)
use_tt_template <- function(date_chr = "2023-10-10",
                            readme = TRUE) {
  # check date in correct format
  if (is.na(as.Date(date_chr, format = "%Y-%m-%d"))) {
    stop("'date_chr' in incorrect format. Should be yyyy-mm-dd.")
  }
  # get year from date
  yr <- sub("-.*", "", date_chr)
  date_strip <- stringr::str_remove_all(date_chr, "-")
  # make folder
  new_folder <- here(yr, date_chr)
  if (!file.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
    message("Created new folder")
  }
  # make new file
  new_file <- here(yr, date_chr, paste0(date_strip, ".R"))
  if (!file.exists(new_file)) {
    file.create(new_file)
    message("Created '.R' file")
    # copy lines to .R file
    r_txt <- readLines(here("template/r-template.R"))
    # replace placeholder text with variables
    r_txt <- gsub(
      pattern = "yr",
      replacement = paste0("\"", yr, "\""),
      x = r_txt
    )
    r_txt <- gsub(
      pattern = "date_chr",
      replacement = paste0("\"", date_chr, "\""),
      x = r_txt
    )
    r_txt <- gsub(
      pattern = "date_strip",
      replacement = paste0("\"", date_strip, "\""),
      x = r_txt
    )
    # write to new file
    writeLines(r_txt, con = new_file)
    message("'.R' contents copied")
  }
  
  if (readme) {
    # make new README file
    new_readme <- here(yr, date_chr, "README.md")
    if (!file.exists(new_readme)) {
      file.create(new_readme)
      message("Created 'README.md' file")
      # copy lines to README file
      readme_txt <- readLines(here("template/readme-template.md"))
      # replace placeholder text with variables
      readme_txt <- gsub(
        pattern = "yr", replacement = yr, x = readme_txt
      )
      readme_txt <- gsub(
        pattern = "date_chr", replacement = date_chr, x = readme_txt
      )
      readme_txt <- gsub(
        pattern = "date_strip", replacement = date_strip, x = readme_txt
      )
      # write to file
      writeLines(readme_txt, con = new_readme)
      message("'README.md' contents copied")
    }
  }
  message("Template successfully copied!")
}
