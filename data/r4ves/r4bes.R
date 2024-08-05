## 0.Confirmation of setting
library("here")

file.exists(here("data", "iris.csv"))
file.exists(here("data", "building.csv"))
file.exists(here("data", "building_meter.csv"))
file.exists(here("data", "idf", "RefBldgMediumOfficeNew2004_Chicago.idf"))
file.exists(here("data", "epw", "USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw"))

## 1.Introduction

## 2.Basics
### 2.1. Prerequisites
install.packages("xml2")
install.packages("tidyverse")

library(tidyverse)
library(data.table)

### 2.2. Basic operators
3+2
3-2
3*2
3/2
3^2
3%/%2
3%%2

value_a <- 2+3
value_b <- 5
value_c <- 3

2+3 == 5
2+3 != 5

(2 + 3 == 5) && (2 + 3 < 3) # logical AND operator
## [1] FALSE
(2 + 3 == 5) || (2 + 3 >= 3) # logical OR operator
## [1] TRUE

x <- c(TRUE, TRUE, FALSE)
y <- c(FALSE, TRUE, FALSE)

x & y
## [1] FALSE  TRUE FALSE
x | y
## [1]  TRUE  TRUE FALSE
!y
## [1]  TRUE FALSE  TRUE

### 2.4. Basic data structures
vec_num <- c(1, 2, 3, 4)
class(vec_num)
## [1] "numeric"

vec_char <- c("R", "for", "BES")
class(vec_char)
## [1] "character"

# coercion if data types are mixed
vec_mix <- c("R", 4, "BES")
class(vec_mix)
## [1] "character"

# you can easily combine vectors using the c function
c(vec_char, vec_mix)
## [1] "R"   "for" "BES" "R"   "4"   "BES"

# vector length
length(vec_num)

## [1] 4
my_list <- list(
    c(1, 2, 3, 4),
    c("a", "b"),
    1L,
    matrix(1:9, ncol = 3)
)

my_list_a <- my_list[1]
class(my_list_a)
## [1] "list"

my_list_b <- my_list[[1]]
class(my_list_b)
## [1] "numeric"

length(my_list)
## [1] 4

named_list <- list(
  a = c(1, 2, 3, 4),
  b = c("a", "b"),
  c = 1L,
  d = matrix(1:9, ncol = 3)
)

class(named_list["a"])
## [1] "list"

class(named_list[["a"]])
## [1] "numeric"

class(named_list$a)
## [1] "numeric"

### 2.5. Tibbles

### To be explored more!!!!!!!!!



## 3.Dates and Times
library(lubridate)

### 3.2.Parsing dates and times
dmy("3/12/1985")


## 4.Importing Data
file.exists(here("data", "iris.csv"))
file.exists(here("data", "building.csv"))

## 4.2.Finding your file
here()
here("data", "iris.csv")

## 4.3.Parsing a csv file
read_csv(here("data", "iris.csv"))

## 4.4.Building data
read_csv(
    here("data","building.csv"),
    col_names = c(
        "datatime",
        "power"
    ),
    skip = 2
)

## 5.Regular Expressions

### To be explored more but complex, not necessary now

## 6.Functions
### 6.3.Examples
cvrmse <- function(meas, pred, p=1){
  se <- (meas - pred)^2
  n <- length(se)
  sqrt(sum(se)/(n-p))/mean(meas) #the last line automaticalle returned
}

cvrmse(c(1,2,3),c(2,4,6))

nmbe <- function(meas, pred, p = 1) {
  be <- (meas - pred)
  n <- length(be)
  (sum(be) / (n - p)) / mean(meas) # last line automatically returned
}

nmbe(c(1, 2, 3), c(2, 4, 6))

## 7.Manipulating Data
### 7.1.Prerequisites

bldg <- read_csv(here("data", "building_meter.csv"))
bldg

names(bldg)

new_name <- str_replace_all(names(bldg), "\\W+", "_")
new_name

names(bldg) <- str_remove_all(new_name, "_Hourly_")
names(bldg)

### 7.2.Data transformation

