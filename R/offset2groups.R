offset2groups <- function(x, offset) {
    x <- as.numeric(x)
    ifelse(x == 1, x - offset, x + offset)
}
