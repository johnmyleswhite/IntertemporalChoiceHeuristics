cross.validate <- function(
    fit.function,
    test.function,
    data,
    proportion,
    epsilon,
    err,
    iterations
) {
    N <- nrow(data)

    results <- data.frame(
        Iteration=1:iterations,
        InSampleError=rep(NA, iterations),
        Error=rep(NA, iterations)
    )

    for (iteration in 1:iterations) {
        train.indices <- sample(1:N, round(N * proportion))

        train.data <- data[train.indices, ]
        test.data <- data[-train.indices, ]

        fitted.model <- fit.function(train.data, epsilon)

        results$InSampleError[iteration] <- test.function(
            train.data,
            fitted.model,
            epsilon,
            err
        )

        results$Error[iteration] <- test.function(
            test.data,
            fitted.model,
            epsilon,
            err
        )
    }

    return(results)
}
