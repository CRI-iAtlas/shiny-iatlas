# Get data from feather files as data.frames and convert them to a tibbles.
driver_mutations1 <-
  feather::read_feather("data2/driver_mutations1.feather")
driver_mutations2 <-
  feather::read_feather("data2/driver_mutations2.feather")
driver_mutations3 <-
  feather::read_feather("data2/driver_mutations3.feather")
driver_mutations4 <-
  feather::read_feather("data2/driver_mutations4.feather")
driver_mutations5 <-
  feather::read_feather("data2/driver_mutations5.feather")
immunomodulator_expr <-
  feather::read_feather("data2/immunomodulator_expr.feather")
io_target_expr1 <-
  feather::read_feather("data2/io_target_expr1.feather")
io_target_expr2 <-
  feather::read_feather("data2/io_target_expr2.feather")
io_target_expr3 <-
  feather::read_feather("data2/io_target_expr3.feather")
io_target_expr4 <-
  feather::read_feather("data2/io_target_expr4.feather")
til_image_links <-
  feather::read_feather("data2/til_image_links.feather")

# Compbine all the sample data.
all_samples <-
  dplyr::bind_rows(
    driver_mutations1,
    driver_mutations2,
    driver_mutations3,
    driver_mutations4,
    driver_mutations5,
    immunomodulator_expr,
    io_target_expr1,
    io_target_expr2,
    io_target_expr3,
    io_target_expr4,
    io_target_expr1
  )

# Get only the sample names (no duplicates).
samples <- all_samples %>% dplyr::distinct(sample)
