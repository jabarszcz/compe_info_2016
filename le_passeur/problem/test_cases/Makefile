AUTOTESTS=input06.txt input07.txt input08.txt input09.txt input10.txt
TESTS=$(wildcard *.txt) $(AUTOTESTS)

all: test_cases.zip

.PHONY: clean

test_cases.zip: $(TESTS:input%.txt=input/input%.txt) $(TESTS:input%.txt=output/output%.txt)
	-@rm test_cases.zip
	zip test_cases.zip {in,out}put/*.txt

input/input%.txt : input%.txt | input
	cp $< input/

input:
	-@mkdir input

output/output%.txt: input/input%.txt | output
	../../solution/solution.py < $< > $@

output:
	-@mkdir output

# random tests
input06.txt:
	./case_generation/generate.py 6 2 6 36 > $@

input07.txt:
	./case_generation/generate.py 18 5 33 64577 > $@

input08.txt:
	./case_generation/generate.py 16 2 14 345 > $@

input09.txt:
	./case_generation/generate.py 25 3 60 9 > $@

input10.txt:
	./case_generation/generate.py 23 2 50 476 > $@

#-------------------------------------------

clean:
	-rm -r input output test_cases.zip $(AUTOTESTS)
