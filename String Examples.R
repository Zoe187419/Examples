#Some examples to dealing with strings in r



#########################################################
#BASICS
#A few ways to enter quotation marks and apostraphes inside a string
line1 <- "The table was a large one, but the three were all crowded together at one corner of it:"
line2 <- '"No room! No room!" they cried out when they saw Alice coming.'
line3 <- "\"There's plenty of room!\" said Alice indignantly, and she sat down in a large arm-chair at one end of the table."

# Putting lines in a vector
lines <- c(line1, line2, line3)
# Print lines
print(lines)
# Use writeLines() on lines
writeLines(lines)
# Write lines with a space separator
writeLines(lines,sep=" ")
# Use writeLines() on the string "hello\n\U1F30D" Note: the last part should
#produce an emoji globe
writeLines("hello\n\U1F30D")

# Should display: To have a \ you need \\
writeLines("To have a \\ you need \\\\")
# Should display: 
# This is a really 
# really really 
# long string
writeLines("This is a really \nreally really \nlong string")
# Use writeLines() with 
# "\u0928\u092e\u0938\u094d\u0924\u0947 \u0926\u0941\u0928\u093f\u092f\u093e" (You just said "Hello World" in Hindi!)
writeLines("\u0928\u092e\u0938\u094d\u0924\u0947 \u0926\u0941\u0928\u093f\u092f\u093e")


#Number formats:
# Some vectors of numbers
percent_change  <- c(4, -1.91, 3.00, -5.002)
income <-  c(72.19, 1030.18, 10291.93, 1189192.18)
p_values <- c(0.12, 0.98, 0.0000191, 0.00000000002)
# Format c(0.0011, 0.011, 1) with digits = 1
format(c(0.0011, 0.011, 1),digits=1)
# Format c(1.0011, 2.011, 1) with digits = 1
format(c(1.0011, 2.011, 1),digits=1)
# Format percent_change to one place after the decimal point
format(percent_change,digits=2)
# Format income to whole numbers
format(income, digits=2)
# Format p_values in fixed format
format(p_values,scientific = FALSE)

formatted_income <- format(income, digits = 2)
# Print formatted_income
print(formatted_income)
# Call writeLines() on the formatted income
writeLines(formatted_income)
# Define trimmed_income
trimmed_income<-format(income,digits=2,trim=TRUE)
# Call writeLines() on the trimmed_income
writeLines(trimmed_income)
# Define pretty_income
pretty_income<-format(income,digits=2,big.mark=",")
# Call writeLines() on the pretty_income
writeLines(pretty_income)


# From the format() exercise
x <- c(0.0011, 0.011, 1)
y <- c(1.0011, 2.011, 1)
# formatC() on x with format = "f", digits = 1
formatC(x,format="f",digits=1)
# formatC() on y with format = "f", digits = 1
formatC(y,format="f",digits=1)
# Format percent_change to one place after the decimal point
formatC(percent_change,format="f",digits=1)
# percent_change with flag = "+"
formatC(percent_change,flag="+",format="f",digits=1)
# Format p_values using format = "g" and digits = 2
formatC(p_values,format="g",digits=2)



# Add $ to pretty_income
paste("$",pretty_income,sep="")
# Add % to pretty_percent
#paste(pretty_percent,"%",sep="")
# Create vector with elements like 2010: +4.0%`
#year_percent <- paste(years,": ",pretty_percent,"%",sep="")
# Collapse all years into single string
#paste(year_percent,collapse=",")

# Define the names vector
income_names <- c("Year 0", "Year 1", "Year 2", "Project Lifetime")
# Create pretty_income
pretty_income <- format(income,digits=2,big.mark=",")
# Create dollar_income
dollar_income <- paste0("$",pretty_income)
# Create formatted_names
formatted_names <- format(income_names,justify="right")
# Create rows
rows<-paste(formatted_names,dollar_income,sep="   ")
# Write rows
writeLines(rows)


# Randomly sample 3 toppings
toppings<-c("cheese","onions","anchovies","mushrooms","sausage")
set.seed(123)
my_toppings <- sample(toppings, size = 3)
# Print my_toppings
print(my_toppings)
# Paste "and " to last element: my_toppings_and
my_toppings_and <- paste0(c("","","and "),my_toppings)
# Collapse with comma space: these_toppings
these_toppings <- paste(my_toppings_and,collapse=", ")
# Add rest of sentence: my_order
my_order <- paste0("I want to order a pizza with ", these_toppings,".")
# Order pizza with writeLines()
writeLines(my_order)






#########################################################
#Introduction to stringr
library(stringr)
library(babynames)

my_toppings <- c("cheese", NA, NA)
my_toppings_and <- paste(c("", "", "and "), my_toppings, sep = "")
# Print my_toppings_and
print(my_toppings_and)
# Use str_c() instead of paste(): my_toppings_str
my_toppings_str <- str_c(c("","","and "),my_toppings)
# Print my_toppings_str
print(my_toppings_str)
# paste() my_toppings_and with collapse = ", "
paste(my_toppings_and,collapse=", ")
# str_c() my_toppings_str with collapse = ", "
str_c(my_toppings_str,collapse=", ")


library(stringr)
library(babynames)
library(dplyr)
# Extracting vectors for boys' and girls' names
babynames_2014 <- filter(babynames, year == 2014)
boy_names <- filter(babynames_2014, sex == "M")$name
girl_names <- filter(babynames_2014, sex == "F")$name
# Take a look at a few boy_names
head(boy_names)
# Find the length of all boy_names
boy_length <- str_length(boy_names)
# Take a look at a few lengths
head(boy_length)
# Find the length of all girl_names
girl_length <- str_length(girl_names)
# Find the difference in mean length
mean(girl_length) - mean(boy_length)
# Confirm str_length() works with factors
head(str_length(factor(boy_names)))


# Extract first letter from boy_names
boy_first_letter <- str_sub(boy_names,1,1)
# Tabulate occurrences of boy_first_letter
table(boy_first_letter)
# Extract the last letter in boy_names, then tabulate
boy_last_letter <- str_sub(boy_names,-1,-1)
table(boy_last_letter)
# Extract the first letter in girl_names, then tabulate
girl_first_letter <- str_sub(girl_names,1,1)
table(girl_first_letter)
# Extract the last letter in girl_names, then tabulate
girl_last_letter <- str_sub(girl_names,-1,-1)
table(girl_last_letter)


# Look for pattern "zz" in boy_names
contains_zz <- str_detect(boy_names,pattern=fixed("zz"))
# Examine str() of contains_zz
str(contains_zz)
# How many names contain "zz"?
sum(contains_zz)
# Which names contain "zz"?
boy_names[contains_zz]


# Look for pattern "zz" in boy_names
contains_zz <- str_detect(boy_names,pattern=fixed("zz"))
# Examine str() of contains_zz
str(contains_zz)
# How many names contain "zz"?
sum(contains_zz)
# Which names contain "zz"?
boy_names[contains_zz]
# Which rows in boy_df have names that contain "zz"?
#boy_df[contains_zz,]


# Find boy_names that contain "zz"
str_subset(boy_names,pattern=fixed("zz"))
# Find girl_names that contain "zz"
str_subset(girl_names,pattern=fixed("zz"))
# Find girl_names that contain "U"
starts_U <- str_subset(girl_names,pattern=fixed("U"))
starts_U
# Find girl_names that contain "U" and "z"
str_subset(starts_U,pattern=fixed("z"))


# Count occurrences of "a" in girl_names
number_as <- str_count(girl_names,pattern=fixed("a"))
# Count occurrences of "A" in girl_names
number_As <- str_count(girl_names,pattern=fixed("A"))
# Histograms of number_as and number_As
hist(number_as)
hist(number_As)
# Find total "a" + "A"
total_as <-number_as+ number_As
# girl_names with more than 4 a's
girl_names[total_as>4]


#Splitting Strings
# Some date data
date_ranges <- c("23.01.2017 - 29.01.2017", "30.01.2017 - 06.02.2017")
# Split dates using " - "
split_dates <- str_split(date_ranges,pattern=fixed(" - "))
split_dates
# Split dates with n and simplify specified
split_dates_n <- str_split(date_ranges,pattern=fixed(" - "),n=2,simplify=TRUE)
split_dates_n
# Subset split_dates_n into start_dates and end_dates
start_dates <- split_dates_n[,1]
end_dates<-split_dates_n[,1]
# Split start_dates into day, month and year pieces
str_split(start_dates,pattern=fixed("."),n=3,simplify=TRUE)


both_names <- c("Box, George", "Cox, David")
# Split both_names into first_names and last_names
both_names_split <- str_split(both_names,pattern=fixed(", "),n=2,simplify=TRUE)
# Get first names
first_names <- both_names_split[,2]
# Get last names
last_names <- both_names_split[,1]



# Split lines into words (variables from beginning of this code)
words <- str_split(lines,pattern=fixed(" "))
# Number of words per line
lapply(words,length)
# Number of characters in each word
word_lengths <- lapply(words,str_length)
# Average word length per line
lapply(word_lengths,mean)


# Some IDs
ids <- c("ID#: 192", "ID#: 118", "ID#: 001")
# Replace "ID#: " with ""
id_nums <- str_replace(ids,pattern=fixed("ID#: "),
                       replacement="")
# Turn id_nums into numbers
id_ints <- as.numeric(id_nums)

# Some (fake) phone numbers
phone_numbers <- c("510-555-0123", "541-555-0167")
# Use str_replace() to replace "-" with " "
str_replace(phone_numbers,fixed("-")," ")
# Use str_replace_all() to replace "-" with " "
str_replace_all(phone_numbers,fixed("-")," ")
# Turn phone numbers into the format xxx.xxx.xxxx
str_replace_all(phone_numbers,fixed("-"),".")


#REVIEW
genes<-c("ATTTTAAACCCCCGGGGGGATCGACTTGACTCGTACTTGATG",
         "ACTACTTGGGGGGGGACTCATTACGACATCTACGACACGATC",
         "CGTACTTGACACTTTCATACTGATGTTAAAAACCGGTTTTTT")
# Find the number of nucleotides in each sequence
str_length(genes)
# Find the number of A's occur in each sequence
str_count(genes,pattern=fixed("A"))
# Return the sequences that contain "TTTTTT"
str_subset(genes,pattern=fixed("TTTTTT"))
# Replace all the "A"s in the sequences with a "_"
str_replace_all(genes,pattern=fixed("A"),"_")


#REVIEW
# Define some full names
names <- c("Diana Prince", "Clark Kent")
# Split into first and last names
names_split <- str_split(names," ",n=2,simplify=TRUE)
# Extract the first letter in the first name
abb_first <- str_sub(names_split[,1],1,1)
# Combine the first letter ". " and last name
str_c(abb_first,". ",names_split[,2])

# Use all names in babynames_2014
all_names <- babynames_2014$name
# Get the last two letters of all_names
last_two_letters <- str_sub(all_names,-2,-1)
# Does the name end in "ee"?
ends_in_ee <- str_detect(last_two_letters,fixed("ee"))
# Extract rows and "sex" column
sex <- unlist(babynames_2014[ends_in_ee,"sex"])
# Display result as a table
table(sex)






#########################################################
#Pattern matching with regular expressions
library(rebus)
# Some strings to practice with
x <- c("cat", "coat", "scotland", "tic toc")
# Print END
print(END)
# Run me
str_view(x, pattern = START %R% "c")
# Match the strings that start with "co" 
str_view(x, pattern = START %R% "co")
# Match the strings that end with "at"
str_view(x, pattern = "at" %R% END)
# Match the string that is exactly "cat"
str_view(x, pattern = START %R% "cat" %R% END)
# Match two characters, where the second is a "t"
str_view(x, pattern = ANY_CHAR %R% "t")
# Match a "t" followed by any character
str_view(x, pattern = "t" %R% ANY_CHAR)
# Match two characters
str_view(x, pattern = ANY_CHAR %R% ANY_CHAR)
# Match a string with exactly three characters
str_view(x, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR %R% END)


#Names with qs
pattern <- "q" %R% ANY_CHAR
# Find names that have the pattern
names_with_q <- str_subset(boy_names,pattern=pattern)
# How many names were there?
length(names_with_q)
# Find part of name that matches pattern
part_with_q <- str_extract(boy_names,pattern=pattern)
# Get a table of counts
table(part_with_q)
# Did any names have the pattern more than once?
count_of_q <- str_count(boy_names,pattern=pattern)
# Get a table of counts
table(count_of_q)
# Which babies got these names?
with_q <- str_detect(boy_names,pattern=pattern)
# What fraction of babies got these names?
mean(with_q)

# Match Jeffrey or Geoffrey
whole_names <- or("Jeffrey", "Geoffrey")
str_view(boy_names, pattern = whole_names, match = TRUE)
# Match Jeffrey or Geoffrey, another way
common_ending <- START %R% or("Je","Geo") %R% "ffrey" %R% END
str_view(boy_names, pattern = common_ending, match = TRUE)
# Match with alternate endings
by_parts <- START %R% or("Je","Geo") %R% "ff" %R% or("ry","ery","rey","erey") %R% END
str_view(boy_names, pattern = by_parts, match = TRUE)
# Match names that start with Cath or Kath
ckath <- START %R% or("Cath","Kath")
str_view(girl_names, pattern = ckath, match = TRUE)













