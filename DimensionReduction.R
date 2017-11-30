Corrmatrix <- structure(c(1, 0.82, 0.54, 0.36, 0.85, 0.82, 1, 0.01, 0.74, 0.36,
                          0.54, 0.01, 1, 0.65, 0.91, 0.36, 0.74, 0.65, 1, 0.36,
                          0.85, 0.36, 0.91, 0.36, 1),
                        .Dim = c(5L, 5L))
Corrmatrix

# returns the highly correlated variables
library(caret)
findCorrelation(Corrmatrix, cutoff = .6, verbose = TRUE)
