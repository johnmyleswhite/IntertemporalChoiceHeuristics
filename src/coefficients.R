source("lib/deps.R")

all.parameters <- data.frame()

for (condition in c(0, 1, 2, 3, 4, 5)) {
    data <- load.data(condition)

    ITCH.fit <- ITCH.fit.function(data)

    parameters <- as.data.frame(summary(ITCH.fit)$coef)

    parameters <- transform(
        parameters,
        Term=c(
            "Other Factors (Beta_I)",
            "Absolute Money (Beta_xA)",
            "Relative Money (Beta_xR)",
            "Absolute Time (Beta_tA)",
            "Relative Time (Beta_tR)"
        )
    )

    parameters <- transform(
        parameters,
        Term=factor(
            Term,
            levels=c(
                "Absolute Money (Beta_xA)",
                "Relative Money (Beta_xR)",
                "Absolute Time (Beta_tA)",
                "Relative Time (Beta_tR)",
                "Other Factors (Beta_I)"
            )
        )
    )

    parameters <- transform(
        parameters,
        LowerBound95=Estimate - 2 * Std..Error
    )

    parameters <- transform(
        parameters,
        UpperBound95=Estimate + 2 * Std..Error
    )

    parameters <- cbind(condition, parameters)

    all.parameters <- rbind(
        all.parameters,
        parameters[
            ,
            c("condition", "Term", "Estimate", "LowerBound95", "UpperBound95")
        ]
    )

    p <- ggplot(parameters, aes(x=Term, y=Estimate, color=Term)) +
        geom_point() +
        geom_pointrange(aes(ymin=LowerBound95, ymax=UpperBound95)) +
        geom_hline(yintercept=0) +
        theme_bw() +
        theme(legend.position="none") +
        xlab("") +
        ylab("Coefficients with 95% Confidence Interval") +
        ylim(-1.5, 1.5) +
        ggtitle("")

    dir.create("graphs/coefficients", recursive=TRUE, showWarnings=FALSE)

    ggsave(
        paste0(
            "graphs/coefficients/model_coefficients",
            condition,
            ".pdf"
        ),
        height=7,
        width=12
    )
}

coef.summ <- function(df) {
    return(
        data.frame(
            Estimate=unique(df$Estimate),
            LowerBoundError=unique(df$LowerBound95),
            UpperBoundError=unique(df$UpperBound95)
        )
    )
}

total.results <- ddply(all.parameters, c("condition", "Term"), coef.summ)

options(digits=6)
print(cast(total.results, condition ~ Term, value="Estimate"))
print(cast(total.results, condition ~ Term, value="LowerBoundError"))
print(cast(total.results, condition ~ Term, value="UpperBoundError"))

total.results$SEM <- total.results$Estimate - total.results$LowerBoundError

all.parameters <- transform(all.parameters, Form=factor(condition))

levels(all.parameters$Form) <- c(
    "Pooled",
    "Absolute $, Delay Framing",
    "Relative $, Delay Framing",
    "Standard MEL",
    "Absolute $, Speedup Framing",
    "Relative $, Speedup Framing"
)

ggplot(
    all.parameters,
    aes(x=Term, y=Estimate, shape=Form, group=Form)
) +
    geom_point() +
    geom_hline(yintercept=0) +
    xlab("") +
    ylab("Decisions Weight") +
    ylim(-1.5, 1.5) +
    ggtitle("") +
    theme_bw()

ggsave("graphs/coefficients/consistent_weights.pdf", height=7, width=12)

ggplot(
    all.parameters,
    aes(x=Term, y=Estimate, shape=Form, group=Form, color=Form)
) +
    geom_point(size=4) +
    geom_line() +
    geom_hline(yintercept=0) +
    xlab("") +
    ylab("Decisions Weight") +
    ylim(-1.5, 1.5) +
    ggtitle("") +
    theme_bw()

ggsave(
    "graphs/coefficients/consistent_weights_with_lines.pdf",
    height=7,
    width=12
)
