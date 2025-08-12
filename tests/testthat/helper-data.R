set.seed(123)

tiny_df <- tibble::tibble(
  group = factor(rep(c("A","B"), each = 6), levels = c("A","B")),
  x_num = c(1:6, 2:7),
  x_cat = rep(c("u","v","v"), 4),
  race___1 = rep(c("Checked","Unchecked"), 6),
  race___2 = rep(c("Unchecked","Checked"), 6)
)

# Checkbox spec used across tests
cb_spec <- tibble::tribble(
  ~var,       ~overall_lbl, ~checkbox_lbl, ~checkbox_txt,
  "race___1", "Race",       "White",       "Checked",
  "race___2", "Race",       "Black",       "Checked"
)