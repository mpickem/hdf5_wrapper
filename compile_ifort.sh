#! /bin/bash

ifort -c hdf5_wrapper.F90 -o hdf5_wrapper.o -I/opt/sw/x86_64/glibc-2.12/ivybridge-ep/hdf5/1.8.12/intel-14.0.2/include
ifort -c test_examples.f90 -o test_examples.o -I/opt/sw/x86_64/glibc-2.12/ivybridge-ep/hdf5/1.8.12/intel-14.0.2/include
ifort test_examples.o hdf5_wrapper.o -o main -I/opt/sw/x86_64/glibc-2.12/ivybridge-ep/hdf5/1.8.12/intel-14.0.2/include -L/opt/sw/x86_64/glibc-2.12/ivybridge-ep/hdf5/1.8.12/intel-14.0.2/lib -lhdf5_fortran -lhdf5hl_fortran

./main
