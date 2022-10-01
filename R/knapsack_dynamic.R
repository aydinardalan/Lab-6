set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
x = knapsack_objects[1:8,]
W = 3500

# knapsack_dynamic <- function(x, W) {
  #check input

  if (!is.data.frame(x)) {stop("x must be data frame")}
  if (any(x<0)) {stop("x must be positive")}
  if (!length(x)==2) {stop("x must have two columns")}
  if (all(names(x)==c("u", "v"))) {stop("x columns' names must be u and v")}
  if (!(W>=0 && length(W)==1 && is.numeric(W))) {"W must be positive numeric with length of 1"}
  
  n <- dim(x)[1]
  M=matrix(nrow = n+1, ncol = W+1)
  v=x$v
  w=x$w
  # for each specific item we step by step increase the knapsack's capacity, and compare the weights
  for (i in 1:n) {
    for (j in 0:W) {
      if (w[i]>j) {M[i+1, j+1]=M[i,j+1]
      }else {
        M[i+1, j+1]= max(M[i,j+1], v[i]+M[i,j+1-w[i]])
      }
    }
  }

  
# }