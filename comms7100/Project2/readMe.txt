compile instructions

To compile everything put all three files in the same folder and run the line
gfortran -o Project2 helpP2.f90 linAlg.f90 Project2.f90
in a compiler. I'm using the minGW compiler from
http://mingw-w64.org/doku.php/download/mingw-builds
but it should work with any fortran compiler, windows or linux.

To run the program use the command line argument
Program2 [file to be processed] [file with peaks]
for the test data we were given that is

assuming that all the files are in the same forlder as the program files
Project2>Project2 proj2_data.txt proj2_bench_frac.txt