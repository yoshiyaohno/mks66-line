all: line.hs
	ghc line.hs -O2
	@echo now make run

run:
	./line
	@echo image rendered to out.ppm

clean:
	-rm line *.hi *.o out.ppm
