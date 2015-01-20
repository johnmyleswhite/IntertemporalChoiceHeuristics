source("lib/deps.R")

set.seed(1)

all.results <- data.frame()

err <- mad
epsilons <- c(0.01)
epsilon <- 1.0

for (epsilon.index in 1:length(epsilons)) {
    epsilon <- epsilons[epsilon.index]
    for (condition in c(0, 1, 2, 3, 4, 5)) {
        data <- load.data(condition)

        N <- 100
        proportion <- 0.75

        baseline.results <- cross.validate(
            baseline.fit.function,
            baseline.test.function,
            data,
            proportion,
            epsilon,
            err,
            N
        )

        DRIFT.results <- cross.validate(
            DRIFT.fit.function,
            DRIFT.test.function,
            data,
            proportion,
            epsilon,
            err,
            N
        )

        ITCH.results <- cross.validate(
            ITCH.fit.function,
            ITCH.test.function,
            data,
            proportion,
            epsilon,
            err,
            N
        )

        tradeoff.results <- cross.validate(
            tradeoff.fit.function,
            tradeoff.test.function,
            data,
            proportion,
            epsilon,
            err,
            N
        )

        exponential.results <- cross.validate(
            exponential.fit.function,
            exponential.test.function,
            data,
            proportion,
            epsilon,
            err,
            N
        )

        homothetic.exponential.results <- cross.validate(
            homothetic.exponential.fit.function,
            homothetic.exponential.test.function,
            data,
            proportion,
            epsilon,
            err,
            N
        )

        hyperbolic.results <- cross.validate(
            hyperbolic.fit.function,
            hyperbolic.test.function,
            data,
            proportion,
            epsilon,
            err,
            N
        )

        homothetic.hyperbolic.results <- cross.validate(
            homothetic.hyperbolic.fit.function,
            homothetic.hyperbolic.test.function,
            data,
            proportion,
            epsilon,
            err,
            N
        )

        quasihyperbolic.results <- cross.validate(
            quasihyperbolic.fit.function,
            quasihyperbolic.test.function,
            data,
            proportion,
            epsilon,
            err,
            N
        )

        homothetic.quasihyperbolic.results <- cross.validate(
            homothetic.quasihyperbolic.fit.function,
            homothetic.quasihyperbolic.test.function,
            data,
            proportion,
            epsilon,
            err,
            N
        )

        baseline.results <- transform(baseline.results, Model="Baseline")

        DRIFT.results <- transform(DRIFT.results, Model="DRIFT")

        ITCH.results <- transform(ITCH.results, Model="ITCH")

        tradeoff.results <- transform(tradeoff.results, Model="Tradeoff")

        exponential.results <- transform(
            exponential.results,
            Model="Exponential"
        )

        homothetic.exponential.results <- transform(
            homothetic.exponential.results,
            Model="Homothetic Exponential"
        )

        hyperbolic.results <- transform(
            hyperbolic.results,
            Model="Hyperbolic"
        )

        homothetic.hyperbolic.results <- transform(
            homothetic.hyperbolic.results,
            Model="Homothetic Hyperbolic"
        )

        quasihyperbolic.results <- transform(
            quasihyperbolic.results,
            Model="Quasi-Hyperbolic"
        )

        homothetic.quasihyperbolic.results <- transform(
            homothetic.quasihyperbolic.results,
            Model="Homothetic Quasi-Hyperbolic"
        )

        results <- rbind(
            exponential.results,
            hyperbolic.results,
            quasihyperbolic.results,
            tradeoff.results,
            DRIFT.results,
            ITCH.results
        )

        ggplot(results, aes(x=Model, y=Error)) +
            stat_summary(fun.data="mean_cl_boot", geom="point") +
            stat_summary(fun.data="mean_cl_boot", geom="errorbar", size=0.2) +
            xlab("Model") +
            ylab("Mean Absolute Deviation under Cross-Validation") +
            ggtitle("") +
            theme_bw() +
            theme(legend.position="none")

        dir.create(
            file.path("graphs", "crossvalidated_comparisons"),
            recursive=TRUE,
            showWarnings=FALSE
        )

        ggsave(
            paste(
                "graphs/crossvalidated_comparisons/",
                "/model_comparisons",
                condition,
                ".pdf",
                sep=""
            )
        )

        results <- rbind(
            baseline.results,
            exponential.results,
            homothetic.exponential.results,
            hyperbolic.results,
            homothetic.hyperbolic.results,
            quasihyperbolic.results,
            homothetic.quasihyperbolic.results,
            tradeoff.results,
            DRIFT.results,
            ITCH.results
        )

        all.results <- rbind(all.results, cbind(condition, results))
    }
}


dir.create("output", recursive=TRUE, showWarnings=FALSE)

write.csv(
    all.results,
    file=file.path("output", "all_results.csv"),
    row.names=FALSE
)

predictive.power <- function(df) {
    res <- NULL

    try(res <- t.test(df$Error))

    if (is.null(res)) {
        return(
            data.frame(
                MeanError=mean(df$Error),
                LowerBoundError=mean(df$Error),
                UpperBoundError=mean(df$Error)
            )
        )
    } else {
        return(
            data.frame(
                MeanError=mean(df$Error),
                LowerBoundError=res$conf.int[1],
                UpperBoundError=res$conf.int[2]
            )
        )
    }
}

total.results <- ddply(
    all.results,
    c("condition", "Model"),
    predictive.power
)

names(total.results) <- c(
    "Condition",
    "Model",
    "MeanError",
    "LowerBoundError",
    "UpperBoundError"
)

total.results <- transform(
    total.results,
    SEM=(UpperBoundError - LowerBoundError) / 2
)

write.csv(
    total.results,
    file=file.path("output", "crossvalidated_results.csv"),
    row.names=FALSE
)

total.results <- transform(
    total.results,
    ErrorSummary=paste(
        MeanError,
        paste("+/-", (UpperBoundError - LowerBoundError) / 2)
    )
)

options(digits=6)

print(cast(total.results, Condition ~ Model, value="ErrorSummary"))
print(cast(total.results, Condition ~ Model, value="MeanError"))
