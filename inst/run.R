# remotes::install_github("DocEd/optimal.rota")
# library(optimal.rota)

library(devtools)
load_all()

# 1. parse preferences
rota <- fit_rota(
  file_path = "~/Documents/clinical/london_clinic/rota/2021/07/",
  fellows = c("matt", "muska", "zainab",
              "dermot", "justin",
              "paul"),
  nhs = c(0, 0, 0, 1, 0, 1), 
  month = 7)

check_fellow(rota, "paul")

export("~/Documents/clinical/london_clinic/rota/2021/07/", rota, assign = 1)
