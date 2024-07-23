dir <- file.path(Sys.getenv("HOME"), ".R")
if (!dir.exists(dir)) dir.create(dir)
writeLines(
  "
    CXX14FLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -Wno-macro-redefined
    CXX14 = $(BINPREF)g++ -m$(WIN) -std=c++1y
    ",
  file.path(dir, "Makevars.win")
)


install.packages("remotes")
remotes::install_github("hongyuanjia/eplusr")
remotes::install_github("ideas-lab-nus/epluspar")
# remotes::install_github("hongyuanjia/epluspar")

#install.packages("eplusr")

# When cannot install epluspar by the above command
# This one should work. It's prebuild binary. So no compilation is needed.
options(repos = c(
  hongyuanjia = 'https://hongyuanjia.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Install some packages
install.packages('epluspar')
