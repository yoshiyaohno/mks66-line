all: line.hs
	ghc -O2 -dynamic line
	@echo now make run

run:
	./line
	@echo image rendered to out.ppm

clean:
	-rm line *.hi *.o out.ppm
