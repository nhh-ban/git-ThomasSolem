
# Skeleton file 2 for Assignment 1 in BAN400. 
# -------------------------------------------

# More detailed steps to complete Problem 1.

library(tidyverse)    # Contains most of what we need.

# Read the entire data file into memory using the readLines()-function. Use the
# URL direcly or read the data from the local file that is in the repository.

# The readLines()-function puts each line into a separate element in a character
# vector. Looking at ?readLines it seems that we only need to provide one
# argument: the "connection", or in other words the location of the text file.
# Replace the "?" below with the location of the file. We do get a warning that
# the file does not end with an "end of line"-character (EOL). This does not
# seem to pose a problem later, and it seems that we can silece the warning by
# switchin off the "warn"-argument. Do that if you wish.

raw_file <- readLines(con = "./suites_dw_Table1.txt", warn = FALSE)

# Identify the line number L of the separator line between the column names and
# the rest of the data table.

# Now every line in the file is in its separate element in the character vector
# "raw_file". The next key step is to identify which line contains the separator
# line between the column names and the rest of the table. We have to come up
# with a rule that defines this line. Let us for example say that "L is the
# first line in the data file that starts with '--'". We can extract the first
# two letters of each of the elements in the "raw_data" vector using the
# substr()-function.

# What do you need to replace the two question marks with in order to extract
# the first two letters?
substr(x = raw_file, start = 0, stop = 2)

# The next step is then to find out *which* line starts with "--", and pick out
# the first one. This can be done in a nice little pipe, where you have to fill
# out the question marks and the missing function names:
find_true <- function(binary) {
  indices <- which(binary == TRUE)
  return(indices)
}





L <- 
  (substr(x = raw_file, start = 0, stop = 2) == "--") %>% 
  find_true %>% 
  min()

# Save the variable descriptions (i.e. the information in lines 1:(L-2)) in a
# text-file for future reference using the cat()-function. The first argument is
# the information that we want to print out. In order to get each element in the
# "raw_file"-vector on a separate line we also provide the sep-argument, where
# we put the "end-of-line"-character "\n". We also need to come up with a file
# name. Replace the question marks:
cat(raw_file[1:12], sep = "\n", file = "discription")

string1 <- cat(raw_file[1:14], sep = "\n")
# Extract the variable names (i.e. line (L-1)), store the names in a vector.

# This is a little bit dirty. We want to *split* the string in raw_data[L-1]
# *by* the character "|", and then we want to *trim* away all the leading and
# trailing white spaces. The first step can be accomplished using the
# str_split()-function in the stringr-package (this is already loaded through
# tidyverse), but there is a delicate detail here. The "|"-character has special
# meaning in R ("or"), so it must be *escaped*, meaning that we tell R that it
# should be interpreted as a normal character. We do that by adding two forward
# slashes in front of it. This function returns a list, with one element for
# each input element. We only send one string in, and hence get only one list
# element out (check that!). We just unlist it to get out the vector. Then we
# apply the str_trim()-function (also in the stringr-package) to get rid of all
# the empty space. Replace the question mark below:

variable_names <- 
  str_split(string = raw_file, pattern = "\\|") %>% 
  unlist() %>% 
  str_trim()

variable_names

# Read the data. One way to do this is to rewrite the data to a new .csv-file
# with comma-separators for instance using cat() again, with the variable names
# from the step above on the first line (see for instance paste() for collapsing
# that vector to a single string with commas as separators).

# Let us try the approach described above. It is quite transparent, but could
# probably be done quicker. We take the elements in "raw_file" containing data,
# replace all "|" with "," and remove all empty space. The gsub-function is
# super for this kind of search-and-replace. Replace the question mark below.

comma_separated_values <- 
  raw_file[L:length(raw_file)] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)

# We then just add the variable names (separated with commas) on top, and
# cat()-the whole ting to a .csv-file in the same way as we did with the
# variable descriptions above.

comma_separated_values_with_names <- 
  c(paste(variable_names, collapse = ","),
    comma_separated_values)

# Replace the question mark and come up with a file name
cat(comma_separated_values_with_names, sep = "\n", file = "prep_gal")

# Read the file back in as a normal csv-file. The readr-package is part of
# tidyverse, so it is already loaded.

galaxies <- read_csv("prep_gal")


# You should now have a nice, clean data frame with galaxies and their
# characteristics in memory. As of March 2022 it should contain 796
# observations.

print(galaxies)

# Task 3 ----

galaxies <- galaxies[-1, ]
#galaxies <- na.omit(galaxies)
galaxies <- galaxies[complete.cases(galaxies$`a_26 - linear diameter of the galaxy in kpc`), ]


# Assuming galaxies is your dataframe

ggplot(galaxies, aes(x = `a_26 - linear diameter of the galaxy in kpc`)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(x = "Linear Diameter (kpc)", y = "Frequency", 
       title = "Distribution of Linear Diameter of Galaxies") +
  theme_minimal() +
  scale_x_continuous(labels = scales::number_format(scale = 0.1))
