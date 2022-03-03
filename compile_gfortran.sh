#! /bin/bash

gfortran -c hdf5_wrapper.F90 -o hdf5_wrapper.o -I/opt/hdf5-1.8.20_gcc/include
gfortran -c test_examples.f90 -o test_examples.o -I/opt/hdf5-1.8.20_gcc/include
gfortran test_examples.o hdf5_wrapper.o -o main -I/opt/hdf5-1.8.20_gcc/include -L/opt/hdf5-1.8.20_gcc/lib -lhdf5_fortran -lhdf5hl_fortran

export LD_LIBRARY_PATH=/opt/hdf5-1.8.20_gcc/lib:$LD_LIBRARY_PATH
./main
