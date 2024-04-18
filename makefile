main: main.f90
	gfortran -framework CoreVideo -framework IOKit -framework Cocoa -framework GLUT -framework OpenGL libraylib.a main.f90 -o main

clean:
	rm -f main
