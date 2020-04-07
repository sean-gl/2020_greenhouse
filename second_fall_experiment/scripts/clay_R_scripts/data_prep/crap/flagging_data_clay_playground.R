require(abind)
test <- as.matrix(weights15[, 1:3], ncol = ncol(weights15))
head(test)
# test[,,2] <- test
test <- array(test, dim = c(dim(test), 1))
flags <- rbinom(nrow(test)*ncol(test), 1, prob = .5)
flags <- as.logical(flags)
flags_array <- array(flags, dim = dim(test))
test_flags <- abind(test, flags_array)
head(test_flags)

# idea: flatten data array, then apply flags
test_vector <- as.vector(test)
test_vector[flags] <- NA
test_new <- matrix(test_vector, nrow = nrow(test), ncol = ncol(test))


