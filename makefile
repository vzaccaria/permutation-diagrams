
.PHONY: test.pdf

test.pdf:
	stack build && stack exec permutation-diagrams-exe -- -o test.pdf -w 100 4 8
	open -a "Skim" test.pdf

all:
	stack build && stack exec permutation-diagrams-exe -- -o foo.pdf -w 300

open:
	open -a "Skim" foo.pdf
