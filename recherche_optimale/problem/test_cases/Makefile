AUTOTESTS=input05.txt input06.txt input07.txt input08.txt input09.txt input10.txt
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

case_generation/generate : case_generation/Main.hs
	(cd ./case_generation; ghc Main.hs -o generate)

# simple tests

input05.txt: mktest5.py
	./mktest5.py 100 > input05.txt

input06.txt: mktest5.py
	./mktest5.py 200 > input06.txt

input07.txt: mktest5.py
	./mktest5.py 220 > input07.txt

# random tests

input08.txt: 
	./case_generation/generate 100 50 50 8 > input08.txt

input09.txt:
	./case_generation/generate 150 100 100 9 > input09.txt

input10.txt:
	./case_generation/generate 200 1000 200 10 > input10.txt

clean:
	-rm -r input output test_cases.zip $(AUTOTESTS)
