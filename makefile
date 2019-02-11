all: line.hs
	ghc line.hs
	@echo now make run

run:
	./line
	@echo image rendered to out.ppm

clean:
	-rm line *.hi *.o out.ppm
