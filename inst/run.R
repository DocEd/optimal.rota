df <- parse_preferences(file_path = "~/Documents/clinical/london_clinic/rota/2020/09/", month = 9)
encode <- encode_preferences(df)
solution <- solve(df, encode)

inspect_consec(solution, df)

export("~/Documents/clinical/london_clinic/rota/2020/09/", df, solution)

df$preferences %>%
  mutate(assignment = df$fellows[df$days[solution@solution[1,]]]) %>%
  write_csv(file.path(file_path, "allocation.csv"))