# remotes::install_github("DocEd/optimal.rota")
# library(optimal.rota)

library(devtools)
load_all()

# 1. parse preferences
rota <- fit_rota(
  selections = "~/Documents/clinical/london_clinic/rota/2021/11/TLC_2021_11.xlsx - Select Preferences.csv",
  requirement = "~/Documents/clinical/london_clinic/rota/2021/11/TLC_2021_11.xlsx - Work Requirement.csv",
  fellows = c("Matt", "Valerie", "Dermot",
              "Justin", "Muska",
              "Paul", "Jenny"),
  nhs = c(0, 0, 1, 0, 0, 1, 0), 
  month = 11)

check_fellow(rota, "matt")

export("~/Documents/clinical/london_clinic/rota/2021/07/", rota, assign = 1)
