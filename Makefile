coefficients:
	Rscript src/coefficients.R
	rm -f Rplots.pdf

consistent_patterns:
	Rscript src/consistent_patterns.R
	rm -f Rplots.pdf

crossvalidated_comparisons:
	Rscript src/crossvalidated_comparisons.R
	rm -f Rplots.pdf

all: coefficients consistent_patterns crossvalidated_comparisons

clean:
	rm -rf graphs
	rm -rf output
