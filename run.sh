# iterate over all *.f files in `src` directory, compile it with gfortran and create a shared library

set -ex

# check if build/src directory exists, if not create it

if [ ! -d "build" ]; then
    mkdir build
fi

if [ ! -d "build/src" ]; then
    mkdir build/src
fi

# iterate over all *.f files in `src` directory
for f in src/*.f; do
    # compile it with gfortran and dump in build/ directory
    gfortran -c $f -o build/${f%.f}.o
done

# create a shared library

# for linux, create *.so file

# gfortran -shared -fPIC -o ./lapack_blas.so build/src/*.o

# execute main.f90

# gfortran -o main main.f90 lapack_blas.so && ./main

# for mac

gfortran -shared -fPIC -o ./lapack_blas.dylib build/src/*.o

# execute main.f90

gfortran -o main main.f90 lapack_blas.dylib && ./main
