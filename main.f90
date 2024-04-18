module raylib
    use iso_c_binding, only: c_char, c_null_char, c_int, c_bool, c_signed_char
    implicit none
    
    type, bind(C) :: color_type
        integer(kind=c_signed_char) :: r
        integer(kind=c_signed_char) :: g
        integer(kind=c_signed_char) :: b
        integer(kind=c_signed_char) :: a
    end type color_type

    interface
        subroutine initwin(width, height, title) bind(C, name="InitWindow")
            use iso_c_binding, only: c_char, c_int
            integer(kind=c_int), value :: width
            integer(kind=c_int), value :: height
            character(kind=c_char) :: title(*)
        end subroutine initwin

        function win_should_close() bind(C, name="WindowShouldClose")
            use iso_c_binding, only: c_bool
            logical(kind=c_bool) :: win_should_close
        end function win_should_close

        subroutine begindraw() bind(C, name="BeginDrawing")
        end subroutine begindraw

        subroutine enddraw() bind(C, name="EndDrawing")
        end subroutine enddraw

        subroutine closewin() bind(C, name="CloseWindow")
        end subroutine closewin

        subroutine drawpixel(posX, posY, color) bind(C, name="DrawPixel")
            use iso_c_binding, only: c_int
            import :: color_type
            integer(kind=c_int), value :: posX
            integer(kind=c_int), value :: posY
            type(color_type), value :: color
        end subroutine drawpixel
    end interface
end module raylib

module mandelbrot
    contains
    pure complex function scalepixel(pixel_x, pixel_y, width, height)
        integer, intent(in) :: pixel_x
        integer, intent(in) :: pixel_y
        integer, intent(in) :: width
        integer, intent(in) :: height
        real :: x
        real :: y

        ! Mandelbrot coordinates
        ! -2,1.12  ... 0.47,1.12
        ! -2,-1.12 ... 0.47,-1.12
        ! x range = 2.47
        ! y range = 2.24
    
        ! 0,0   ... 800,0   ->  0,0 ... 1,0 -> 0,0  ... 1,0  -> 0,1 ... 1,1
        ! 0,500 ... 800,500     0,1 ... 1,1    0,-1 ... 1,-1    0,0 ... 1,0

        x = (( real(pixel_x) / width ) * 2.47 ) - 2.0
        y = ((( -real(pixel_y) / height ) + 1.0 ) * 2.24 ) - 1.12
        scalepixel = complex(x, y)
    end function

    pure logical function escapes(z0) 
        complex, intent(in) :: z0
        complex :: z
        integer :: max_iterations
        integer :: iteration
        
        z = 0
        iteration = 0
        max_iterations = 1000
        do while (cabs(z) < 4 .and. iteration < max_iterations)
            z = z*z + z0
            iteration = iteration + 1
        enddo
        
        if (iteration >= max_iterations) then

            escapes = .true.
        else
            escapes = .false.
        end if
    end function escapes

    function fillmatrix(width, height)
        integer, intent(in) :: width
        integer, intent(in) :: height
        logical :: fillmatrix(width, height)
        integer :: pix_x, pix_y
        complex :: z
        
        do pix_x = 1, width
            do pix_y = 1, height
                z = scalepixel(pix_x, pix_y, width, height)
                fillmatrix(pix_x,pix_y) = escapes(z)
            enddo
        enddo
    end function fillmatrix
end module mandelbrot

program draw_rect
    use raylib
    use mandelbrot
    use iso_c_binding
    implicit none

    integer(c_int) :: width, height
    integer(c_int) :: pix_x, pix_y
    logical, dimension(800, 500) :: does_escape

    width = 800
    height = 500

    does_escape = fillmatrix(width, height)

    call initwin(width, height, "Mandelbrot Set"//c_null_char)

    do while (.not. win_should_close())
        call begindraw()

        do pix_x = 1, width
            do pix_y = 1, height
                if (does_escape(pix_x, pix_y)) then
                    call drawpixel(pix_x, pix_y, color_type(127, 40, 40, 127))
                else
                    call drawpixel(pix_x, pix_y, color_type(0, 0, 0, 127))
                end if
            enddo
        enddo

        call enddraw()
    enddo
    
    call closewin()

end program
