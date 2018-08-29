#! /bin/bash

gfortran -c hdf5_wrapper.f90 -o hdf5_wrapper.o -I/opt/hdf5-1.8.16_gcc/include -L/opt/hdf5-1.8.16_gcc/lib -lhdf5_fortran -lhdf5hl_fortran
gfortran -c test_examples.f90 -o test_examples.o -I/opt/hdf5-1.8.16_gcc/include -L/opt/hdf5-1.8.16_gcc/lib -lhdf5_fortran -lhdf5hl_fortran
gfortran test_examples.o hdf5_wrapper.o -o main -I/opt/hdf5-1.8.16_gcc/include -L/opt/hdf5-1.8.16_gcc/lib -lhdf5_fortran -lhdf5hl_fortran -fbounds-check -Wall -fbacktrace

./main

