.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

# .PHONY : usage
# usage:
# 	@echo ""
# 	@echo "    * USAGE * "
# 	@echo ""
# 	@echo ""

objdir = .obj

# Command-line options at make call
ifort ?= 1
debug ?= 0
opemp ?= 0

ifeq ($(ifort), 0)
FC = gfortran
else
FC = ifort
endif


ifeq ($(ifort), 0)
LIBSYS = /usr/lib
INCSYS = /usr/include

LIBNETCDF = -I $(INCSYS) -L$(LIBSYS) -lnetcdff -lnetcdf
LIBNCIO   = -I../ncio/.obj -L../ncio -lncio
else
LIBSYS = -L/usr/lib64 -lnetcdf -lnetcdff -L/lib64
INCSYS = -I/usr/local/install/netcdf-4.3.2/include

LIBNETCDF = $(INCSYS) $(LIBSYS)
LIBNCIO   = -Wl,-rpath=../iloveclim/ncio -I../iloveclim/ncio/.obj -L../iloveclim/ncio/library -lncio
endif

SRCFAGS =
OBJFAGS = -J$(objdir) -I$(objdir)
BINFAGS =

LIBS= $(LIBNCIO) $(LIBNETCDF)

ifeq ($(ifort), 0)
DFLAGS = -O3 -cpp -ffixed-line-length-132 -fno-align-commons
ifeq ($(debug), 1)
    DFLAGS  = -g -pg -cpp -Wall -ffixed-line-length-132 -fno-align-commons
# -w -p -ggdb -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all
endif
else

## IFORT OPTIONS ##

ifeq ($(opemp), 1)
     OMPFLAGS        = -openmp -openmp-link static -auto-scalar -norecursive
else
     OMPFLAGS        =
endif
    FC = ifort
    LIBNETCDF = $(INCSYS) $(LIBSYS)

    FLAGS        = -module $(objdir) -L$(objdir) -I$(INC)
    DFLAGS       = -cpp -vec-report0 -132 -r8 $(OMPFLAGS)
    ifeq ($(debug), 1)
         DFLAGS   = -cpp -vec-report0 -132 -r8 -C -traceback -ftrapuv -fpe0 -check all -vec-report0 $(OMPFLAGS)
         # -w
    endif
    OBJFAGS = -I$(objdir)
endif

SRC     = \
	calcmean.f90\
	readVarClimato.f90\
	distance_great_circle.f90\
	interpolate_mod.f90



OBJS    = $(SRC:.f90=.o)
MODS    = ${SRC:.f90=.mod}
OBJPATH = $(addprefix $(objdir)/, $(OBJS))

## Individual libraries or modules ##
# %.mod %.o : %.f90
%.o : %.f90
	$(FC) $(DFLAGS) $(FLAGS) ${LIBS} $(OBJFAGS) -o $(objdir)/$@ -c $<
	@echo "" $(OBJPATH)

%.mod : %.f90 %.o
	@true

## Shared library 
# .so: %.f90
# 	$(FC) -c -shared -fPIC $(DFLAGS) $(FLAGS) ${LIBS} -o ncio.so $<

## Complete programs

main: $(OBJS)
	$(FC) $(DFLAGS) -o test.x $(OBJFAGS) $(OBJPATH) driver-3.f90 $(LIBS)
	@echo " "
	@echo "    test.x is ready."
	@echo " "
clean:
	rm -f test.x $(objdir)/*.o $(objdir)/*.mod $(objdir)/*.so
