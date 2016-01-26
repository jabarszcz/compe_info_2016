AUTOTESTS=input00.txt input01.txt input02.txt input03.txt input04.txt input05.txt input06.txt input07.txt input08.txt input09.txt input10.txt
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

# handwritten tests (no recipe)

# random tests
case_generation/generate : case_generation/Main.hs
	(cd ./case_generation; ghc Main.hs -o generate)

input00.txt: case_generation/generate
	./case_generation/generate 3 1 3 107337 > $@

input01.txt: case_generation/generate
	./case_generation/generate 1 1 1 0 > $@

input02.txt: case_generation/generate
	./case_generation/generate 10 1 100 1 > $@

input03.txt: case_generation/generate
	./case_generation/generate 100 1 100 2 > $@

input04.txt: case_generation/generate
	./case_generation/generate 100 1 1000000 3 > $@

input05.txt: case_generation/generate
	./case_generation/generate 1000 1 1000000 4 > $@

input06.txt: case_generation/generate
	./case_generation/generate 1000 1 1000000 5 > $@

input07.txt: case_generation/generate
	./case_generation/generate 10000 1 100000000 6 > $@

input08.txt: case_generation/generate
	./case_generation/generate 20000 1 100000000000 7 > $@

input09.txt: case_generation/generate
	./case_generation/generate 20000 1 1000000000000000 8 > $@

input10.txt: case_generation/generate
	./case_generation/generate 100000 1 1000000 9 > $@

#-------------------------------------------

clean:
	-rm -r input output test_cases.zip $(AUTOTESTS)
	-(cd case_generation; rm generate *.hi *.o)
	-(cd ../../solution; rm solution *.hi *.o)
