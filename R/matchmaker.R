library(dplyr)
library(tidyr)

df1 <- tibble(
  gurken = c("Bungas (gut)", "Bungas", "Heinz", "Heinz Maky", "Birgit", "Bungas Hungas", "Birgit")
)

mx <- tibble(
  typ = c("Bungas", "Heinz", "Heinz Maky", "Birgit")
)

rescale01 <- function(x) (x - min(x))/(max(x)-min(x))
rescale10 <- function(x) 1 - rescale01(x)
rescale_modify <- function(x, modification = c("none", "pull up", "push down")) {
  # check if argument between 0 and 1
  if (min(x) < 0 | max(x) > 1) stop("Please provide numeric between 0 and 1.")
  # define helper function to deal with 0 and 1
  massage <- function(x) {
    # define arbitrary small number
    s <- 1e-8
    # replace zeroes with s
    x[x == 0] <- s
    # replace ones with 1-s
    x[x == 1] <- 1 - s
    # return
    x
  }
  # check modification
  md <- match.arg(modification)
  # return based on md
  switch (md,
          none = x,
          `pull up` = rescale01(log(massage(x))),
          `push down` = rescale01(-log(1 - massage(x)))
  )
}

rescale_adist <- function(x) rescale_modify(rescale10(x), "push down")

rs <- adist(mx$typ, df1$gurken, fixed = F, ignore.case = T)
#  t(apply(rs, 1, rescale10)) * matrix(nchar(mx$typ), ncol=1) %*% (1/nchar(df1$gurken))

# t(apply(rs, 1, rescale_adist)) * matrix(nchar(mx$typ), ncol=1) %*% (1/nchar(df1$gurken))

matches <- t(apply(rs, 1, rescale_adist)) * matrix(nchar(mx$typ), ncol=1) %*% (1/nchar(df1$gurken))

matches_d <- matches %>%
  as.data.frame %>%
  mutate(typ = mx$typ) %>%
  pivot_longer(starts_with("V"),
               names_to = "Zeile",
               values_to = "Score") %>%
  filter(`Score` > 0.1) %>%
  group_by(Zeile) %>%
  slice_max(`Score`, n = 1, with_ties = FALSE) %>% # do not allow more than one match
  arrange(`Zeile`)


