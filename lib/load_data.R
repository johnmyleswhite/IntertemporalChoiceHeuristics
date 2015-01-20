load.data <- function(condition.number) {
	data <- read.csv(
        "data/choices.csv",
        stringsAsFactors = FALSE
    )

    # Drop rows with missing responses
	data <- subset(data, !is.na(LaterOptionChosen))

    # Select the subset of data for this condition, where 0 is pooled over all.
	if (condition.number != 0) {
		data <- subset(data, Condition == condition.number)
	} else {
		data <- data
	}

	# Add additional features required by ITCH model
	data <- transform(data, XStar=(X1 + X2) / 2)
	data <- transform(data, TStar=(T1 + T2) / 2)
	data <- transform(data, G=scale(X2 - XStar))
	data <- transform(data, R=scale((X2 - X1) / XStar))
	data <- transform(data, D=scale(T2 - TStar))
	data <- transform(data, T=scale((T2 - T1) / TStar))

    # Add additional features required by DRIFT model
	data <- transform(data, DriftD=scale(X2 - X1))
	data <- transform(data, DriftR=scale((X2 - X1) / X1))
	data <- transform(data, DriftI=scale((X2 / X1)^(1 / (T2 - T1)) - 1))
	data <- transform(data, DriftT=scale(T2 - T1))

	# Rescale data when working with "raw" numbers
	data <- transform(data, X1=X1 / max(X2))
	data <- transform(data, X2=X2 / max(X2))

	return(data)
}
