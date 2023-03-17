# Makefile for Gau_vibr2xyz
SHELL := cmd
FC := gfortran
FLINKER := $(FC)
LAPACKROOT := C:\\lapack-3.10.1
LIBPATH := -L $(LAPACKROOT)\\lib
LIB := -l lapack -l blas

FILENAME := Gau_vibr2xyz

.PHONY: all

all: $(FILENAME)

.PHONY: $(FILENAME)

$(FILENAME): $(FILENAME).exe

%.exe: %.obj
	@echo Linking $@ against LAPACK and BLAS ...
	$(FLINKER) -o $@ $^ $(LIBPATH) $(LIB) -static -s

%.obj: %.f90
	@echo Compiling $@ ...
	$(FC) -o $@ -c $< -s

.PHONY: veryclean clean

veryclean: clean
	-del /q $(FILENAME).exe 2> NUL

clean:
	-del /q $(FILENAME).obj 2> NUL

