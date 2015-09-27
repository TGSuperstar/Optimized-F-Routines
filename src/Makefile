FC=gfortran
FFLAGS=-O3 -lblas

#FC=ifort
#FFLAGS= -O3

MAIN=main
OBJS= \
	constants.o \
	timer.o \
	MatMat.o \
	main.o

.SUFFIXES: .F90


$(MAIN): $(OBJS)
	$(FC) $(FFLAGS) -o $(MAIN) $(OBJS)
#	$(FC) $(FFLAGS) -o $(MAIN) MatMat.o -L$(MKLPATH) -I$(MKLINCLUDE) -lmkl_intel -lmkl_intel_thread -lmkl_core -lomp5 -lpthread -openmp

.F90.o:
	$(FC) $(FFLAGS) $(INCLUDES) -c $<  -o $@

clean:
	rm -f $(MAIN) $(OBJS) *.mod

check:
	./main
