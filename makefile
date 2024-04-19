main: main.f90
	gfortran -fno-range-check -framework CoreVideo -framework IOKit -framework Cocoa -framework GLUT -framework OpenGL libraylib.a main.f90 -o main

clean:
	rm -f main
