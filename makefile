

all:
	stack build && stack exec permutation-diagrams-exe -- -o foo.pdf -w 300

open:
	open -a "Skim" foo.pdf
