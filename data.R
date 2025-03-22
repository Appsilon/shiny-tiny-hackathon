library(data.table)
library(openxlsx2)

read.dat <- function(file) {
  dat <- read_xlsx(
    file = file,
    na.strings = "-"
  ) |> setDT()

  cat(paste(names(dat)[-c(1, 3)], collapse = ", "), "\n")

  tab.name <- names(dat)[2]

  # setnames(dat, tolower)

  dat <- dat[-1]

  dat[, Year := as.integer(Year)]

  # Delete columns 2 and 3
  dat <- dat[, -c(2, 3)]

  dat <- melt.data.table(
    data = dat,
    id.vars = "Year",
    na.rm = TRUE,
    variable.name = "Variable",
    value.name = "Value"
  )

  dat[, Table := tab.name]

  setkey(dat, Table, Year, Variable)
  setcolorder(dat)

  return(dat[])

}

read.dat("data/9c4517b9-0535-47bc-a55b-b7d2da9a4edd.xlsx")
read.dat("data/2f29e29b-9819-418d-a0ce-5eb14be3d9d5.xlsx")

dat <- purrr::map(
  .x = list.files("data", pattern = "\\.xlsx$", full.names = TRUE),
  .f = read.dat
) |> rbindlist()

fwrite(dat, "merged.csv")

dat[, unique(Table)]

dat[Table == "Seriousness", sum(Value), keyby = .(Variable)]
dat[Table == "Seriousness", sum(Value)]
dat[Table == "Seriousness" & Variable == "Serious", sum(Value)]
dat[Table == "Seriousness" & Variable == "Death", sum(Value)]

dat[, .(`Total Reports` = sum(Value)), keyby = .(Table, Variable)]
dat[, .(`Total Reports` = sum(Value)), keyby = .(Year)]
