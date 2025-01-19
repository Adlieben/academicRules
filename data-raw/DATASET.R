# Example of minimal IB-like data
SynthIB <- data.frame(
  student_id = c("S001","S002","S003","S004"),
  CAS_met    = c(TRUE, TRUE, FALSE, TRUE),
  Total      = c(25, 20, 27, 30),
  lowest_grade = c(3, 2, 4, 4),
  count_2    = c(0, 1, 0, 0),
  count_3orbelow = c(1, 2, 0, 0),
  HL_sum     = c(12, 8, 15, 15),
  SL_sum     = c(11, 10, 10, 12),
  n_HL       = c(3, 3, 3, 3),
  n_SL       = c(3, 3, 3, 3)
)

usethis::use_data(SynthIB, overwrite = TRUE)
