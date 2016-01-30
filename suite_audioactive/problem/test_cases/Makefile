AUTOTESTS=input04.txt input05.txt input06.txt input07.txt input08.txt input09.txt input10.txt
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

../../solution/solution: ../../solution/Main.hs
	(cd ../../solution/; ghc Main.hs -o solution)

output/output%.txt: input/input%.txt ../../solution/solution | output
	../../solution/solution < $< > $@

output:
	-@mkdir output

# random tests
case_generation/generate : case_generation/Main.hs
	(cd ./case_generation; ghc Main.hs -o generate)

input04.txt: case_generation/generate
	./case_generation/generate 100 9 4 > $@

input05.txt: case_generation/generate
	./case_generation/generate 1000 11 5 > $@

input06.txt: case_generation/generate
	./case_generation/generate 1000 5 6 > $@

input07.txt: case_generation/generate
	./case_generation/generate 10000 6 7 > $@

input08.txt: case_generation/generate
	./case_generation/generate 20000 7 8 > $@

input09.txt: case_generation/generate
	./case_generation/generate 20000 5 9 > $@

input10.txt: case_generation/generate
	./case_generation/generate 100000 3 10 > $@

#-------------------------------------------

clean:
	-rm -r input output test_cases.zip $(AUTOTESTS)
