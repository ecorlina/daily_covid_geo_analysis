library(tidyverse)

left_table <- tribble(
   ~idnum, ~label, ~area,
   #——————|———————|-----
   1, "alpha", 25,
   2, "beta", 10,
   2, "beta", 5,
   3, "gamma", 18,
   4, "delta", 12,
   4, "delta", 4
)

right_table <- tribble(
   ~idnum, ~label, ~pop,
   #——————|———————|-----
   1, "alpha", 200,
   2, "beta", 300,
   3, "gamma", 150,
   4, "delta", 280
)

right_table_2 <- tribble(
   ~label, ~pop1, ~pop2, ~pop3,
   #——————|------|------|------
   "alpha", 200, 300, 400,
   "beta", 300, 350, 375,
   "gamma", 150, 200, 300,
   "delta", 280, 320, 360
)



area_totals <- left_table %>%
   group_by(label) %>%
   summarize(total_area = sum(area))

left_table_2 <- left_join(left_table, area_totals, by = ("label"))

left_table_2 <- left_table_2 %>%
   mutate(area_pct = area / total_area)

new_table <- left_join(left_table_2, right_table)

new_table <- new_table %>%
   mutate(split_pop = area_pct * pop)

another_new_table <-  left_join(left_table_2, right_table_2)

got_it <- another_new_table %>%
   mutate_at(.vars = vars(starts_with("pop")),
             .funs = list(~ (. * area_pct)))

