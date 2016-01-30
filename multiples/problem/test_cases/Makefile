AUTOTESTS=input01.txt input02.txt input03.txt input04.txt input05.txt input06.txt input07.txt input08.txt
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

input01.txt: case_generation/generate
	./case_generation/generate 2 5 30 100000 1000000 1 1000000000 1 > $@

input02.txt: case_generation/generate
	./case_generation/generate 12 6 300 100000 1000000 1 1000000000 2 > $@

input03.txt: case_generation/generate
	./case_generation/generate 3 4 100000 1 100 100 1000000 3 > $@

input04.txt: case_generation/generate
	./case_generation/generate 9 7 35000 1000000 10000000 1 1000000000 4 > $@

input05.txt: case_generation/generate
	./case_generation/generate 2 2 35000 1000000 10000000 10 1000000000 5 > $@

input06.txt: case_generation/generate
	./case_generation/generate 5 4 100000 1000 1000000 10 1000000000 6 > $@
input07.txt: case_generation/generate
	./case_generation/generate 5 4 100000 1000 1000000 10 1000000000 7 > $@
input08.txt: case_generation/generate
	./case_generation/generate 5 4 100000 1000 1000000 10 1000000000 8 > $@

#-------------------------------------------

clean:
	-rm -r input output test_cases.zip $(AUTOTESTS)
