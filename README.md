# Optimal.Rota

Optimal matching for rota management.

```r
rota <- fit_rota(
  selections = "path_to_file/Select_Preferences.csv",
  requirement = "path_to_file/Work_Requirement.csv",
  fellows = c("Matt", "Valerie", "Dermot",
              "Justin", "Muska",
              "Paul", "Jenny"),
  nhs = c(0, 0, 1, 0, 0, 1, 0), 
  month = 11)

check_fellow(rota, "paul")

export("~/Documents/clinical/london_clinic/rota/2021/11/", rota, assign = 1)
```