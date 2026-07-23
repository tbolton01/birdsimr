df <- df %>%
  group_by(Terr) %>%
  mutate(
    meanTerr = mean(CY_Prod)
  ) %>%
  ungroup()
var_beta <- function(a, b) {
  vb <- (a*b)/((a+b)^2*(a+b+1))
  return(sqrt(vb))
}

library(readr)
df <- read_csv("E:/Goshawk/wallchart_w_elev.csv")
df <- df[!is.na(df$breeding.exp),]
df <- df %>%
  group_by(as.factor(BandID)) %>%
  mutate(
    breed_span = max(breeding.exp)
  ) %>%
  ungroup()
summary(df$breed_span)  

df <- df %>%
  distinct(BandID, breed_span)