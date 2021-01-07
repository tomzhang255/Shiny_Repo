mat1 <- matrix(
  c(
    0, 1, 0, 0, 1,
    1, 0, 1, 0, 1,
    0, 1, 0, 1, 0,
    0, 0, 1, 0, 1,
    1, 1, 0, 1, 0
  ),
  byrow = T,
  nrow = 5)

write.table(mat1, file = "mat1.csv", sep = ",", row.names = F, col.names = F)



mat2 <- matrix(
  c(
    0, 0, 1, 1, 1,
    0, 0, 0, 1, 1,
    1, 0, 0, 1, 0,
    1, 1, 1, 0, 0,
    1, 1, 0, 0, 0
  ),
  byrow = T,
  nrow = 5)

write.table(mat2, file = "mat2.csv", sep = ",", row.names = F, col.names = F)



mat3 <- matrix(
  c(
    0, 1, 1, 0, 0, 0,
    1, 0, 1, 1, 0, 0,
    1, 1, 0, 0, 0, 0,
    0, 1, 0, 0, 1, 1,
    0, 0, 0, 1, 0, 1,
    0, 0, 0, 1, 1, 0
  ),
  byrow = T,
  nrow = 6)

write.table(mat3, file = "mat3.csv", sep = ",", row.names = F, col.names = F)



mat4 <- matrix(
  c(
    0, 1, 0, 0, 0, 1,
    1, 0, 1, 0, 1, 0,
    0, 1, 0, 1, 0, 0,
    0, 0, 1, 0, 1, 0,
    0, 1, 0, 1, 0, 1,
    1, 0, 0, 0, 1, 0
  ),
  byrow = T,
  nrow = 6)

write.table(mat4, file = "mat4.csv", sep = ",", row.names = F, col.names = F)


