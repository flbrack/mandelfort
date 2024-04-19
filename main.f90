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
        subroutine init_win(width, height, title) bind(C, name="InitWindow")
            use iso_c_binding, only: c_char, c_int
            integer(kind=c_int), value :: width
            integer(kind=c_int), value :: height
            character(kind=c_char) :: title(*)
        end subroutine init_win

        function win_should_close() bind(C, name="WindowShouldClose")
            use iso_c_binding, only: c_bool
            logical(kind=c_bool) :: win_should_close
        end function win_should_close

        subroutine begin_draw() bind(C, name="BeginDrawing")
        end subroutine begin_draw

        subroutine end_draw() bind(C, name="EndDrawing")
        end subroutine end_draw

        subroutine close_win() bind(C, name="CloseWindow")
        end subroutine close_win

        subroutine draw_pixel(posX, posY, color) bind(C, name="DrawPixel")
            use iso_c_binding, only: c_int
            import :: color_type
            integer(kind=c_int), value :: posX
            integer(kind=c_int), value :: posY
            type(color_type), value :: color
        end subroutine draw_pixel
    end interface
end module raylib

module mandelbrot
    use raylib
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

     pure type(color_type) function escapes(z0) 
        complex, intent(in) :: z0
        complex :: z
        integer :: max_iterations
        integer :: iteration
        integer :: scaled_iter
        
        z = 0
        iteration = 0
        max_iterations = 100
        do while (cabs(z) < 2 .and. iteration < max_iterations)
            z = z*z + z0
            iteration = iteration + 1
        enddo
        
        if (iteration >= max_iterations) then
            escapes = color_type(0, 0, 0, 255)
        else
            scaled_iter = int(255*(1 - exp(-5*real(iteration)/max_iterations)))
            escapes = color_type(scaled_iter, scaled_iter, 255-scaled_iter, 255)
        end if
    end function escapes

    function fill_color_matrix(width, height)
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(color_type) :: fill_color_matrix(width, height)
        integer :: pix_x, pix_y
        complex :: z
        
        do pix_x = 1, width
            do pix_y = 1, height
                z = scalepixel(pix_x, pix_y, width, height)
                fill_color_matrix(pix_x,pix_y) = escapes(z)
            enddo
        enddo
    end function fill_color_matrix
end module mandelbrot

program draw_rect
    use raylib
    use mandelbrot
    use iso_c_binding
    implicit none

    integer(c_int), parameter :: width = 800
    integer(c_int), parameter :: height = 500
    integer(c_int) :: pix_x, pix_y
    type(color_type), dimension(width, height) :: color_matrix

    color_matrix = fill_color_matrix(width, height)

    call init_win(width, height, "Mandelbrot Set"//c_null_char)

    do while (.not. win_should_close())
        call begin_draw()

        do pix_x = 1, width
            do pix_y = 1, height
                call draw_pixel(pix_x, pix_y, color_matrix(pix_x, pix_y))
            enddo
        enddo

        call end_draw()
    enddo
    
    call close_win()

end program
