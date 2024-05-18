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

        subroutine draw_text(text, posX, posY, fontsize, color) bind(C, name="DrawText")
            use iso_c_binding, only: c_char, c_int
            import :: color_type
            character(kind=c_char) :: text(*)
            integer(kind=c_int), value :: posX
            integer(kind=c_int), value :: posY
            integer(kind=c_int), value :: fontsize
            type(color_type), value :: color
        end subroutine draw_text
    end interface
end module raylib

module mandelbrot
    use raylib, only: color_type
    contains
    pure complex function scale_pixel(pixel_x, pixel_y, width, height, x_lo, x_hi, y_lo, y_hi)
        integer, intent(in) :: pixel_x
        integer, intent(in) :: pixel_y
        integer, intent(in) :: width
        integer, intent(in) :: height
        real, intent(in) :: x_lo
        real, intent(in) :: x_hi
        real, intent(in) :: y_lo
        real, intent(in) :: y_hi
        real :: x_range, y_range
        real :: r, i
        ! 0,0      ... width,0      ->  0,0 ... 1,0 -> 0,0  ... 1,0  -> 0,1 ... 1,1
        ! 0,height ... width,height     0,1 ... 1,1    0,-1 ... 1,-1    0,0 ... 1,0
        x_range = x_hi - x_lo
        y_range = y_hi - y_lo
        r = (( real(pixel_x) / width ) * x_range ) + x_lo
        i = ((( -real(pixel_y) / height ) + 1.0 ) * y_range ) + y_lo
        scale_pixel = complex(r, i)
    end function scale_pixel

    function create_palette(n)
        integer, intent(in) :: n
        type(color_type) :: create_palette(n)
        integer :: i

        do i = 1, n
            if (i < 255) then
                ! (0, 0, 255) -> (255, 0, 0)
                create_palette(i) = color_type(i, 0, 255-i, 255)
            else if (i < 510) then
                ! (255, 0, 0) -> (0, 255, 0)
                create_palette(i) = color_type(510-i, i-255, 0, 255)
            else
                ! (0, 255, 0) -> (0, 255, 255)
                create_palette(i) = color_type(0, 255, i-510, 255)
            end if
        enddo
    end function create_palette

    pure type(color_type) function escapes(z0, palette, palette_size) 
        complex, intent(in) :: z0
        type(color_type), intent(in) :: palette(*)
        integer, intent(in) :: palette_size
        complex :: z
        integer :: max_iter, iter, scaled_iter
        
        z = 0
        iter = 0
        max_iter = 200
        do while (cabs(z) < 2 .and. iter < max_iter)
            z = z*z + z0
            iter = iter + 1
        enddo
        
        if (iter >= max_iter) then
            escapes = color_type(0, 0, 0, 255)
        else
            ! scaled_iter = int(255*(1 - exp(-2*real(iter)/max_iter)))
            scaled_iter = int(mod((( (real(iter)/max_iter) ** 2.0) * palette_size) ** 1.5, real(palette_size)))
            ! escapes = color_type(127-scaled_iter, scaled_iter, 255-scaled_iter, 255)
            escapes = palette(scaled_iter)
        end if
    end function escapes

    function fill_color_matrix(width, height, x_lo, x_hi, y_lo, y_hi, palette, palette_size)
        integer, intent(in) :: width
        integer, intent(in) :: height
        real, intent(in) :: x_lo
        real, intent(in) :: x_hi
        real, intent(in) :: y_lo
        real, intent(in) :: y_hi
        integer, intent(in) :: palette_size
        type(color_type) :: palette(*)
        type(color_type) :: fill_color_matrix(width, height)
        integer :: pix_x, pix_y
        complex :: z
        
        do pix_x = 1, width
            do pix_y = 1, height
                z = scale_pixel(pix_x, pix_y, width, height, x_lo, x_hi, y_lo, y_hi)
                fill_color_matrix(pix_x, pix_y) = escapes(z, palette, palette_size)
            enddo
        enddo
    end function fill_color_matrix
end module mandelbrot

program draw_rect
    use raylib
    use mandelbrot
    use iso_c_binding
    implicit none

    integer(c_int), parameter :: width = 1000
    integer(c_int), parameter :: height = 800
    integer(c_int) :: pix_x, pix_y
    type(color_type), dimension(width, height) :: color_matrix
    integer, parameter :: palette_size = 765
    type(color_type), dimension(palette_size) :: palette
    logical, parameter :: debug = .true.
    character(50) :: dbgstr1, dbgstr2, dbgstr3, dbgstr4
    real, parameter :: x_lo = 0.012                                      ! Mandelbrot limits
    real, parameter :: x_hi = 0.0125                                      ! -2,1.12  ... 0.47,1.12
    real, parameter :: y_lo = 0.65                                      ! -2,-1.12 ... 0.47,-1.12
    real, parameter :: y_hi = y_lo + (x_hi - x_lo)*(real(height)/width)

    palette = create_palette(palette_size)
    color_matrix = fill_color_matrix(width, height, x_lo, x_hi, y_lo, y_hi, palette, palette_size)

    call init_win(width, height, "Mandelbrot Set"//c_null_char)
    do while (.not. win_should_close())
        call begin_draw()

        do pix_x = 1, width
            do pix_y = 1, height
                call draw_pixel(pix_x, pix_y, color_matrix(pix_x, pix_y))
            enddo
        enddo

        if (debug) then
            write (dbgstr1, '(f10.7)') x_lo
            write (dbgstr2, '(f10.7)') x_hi
            write (dbgstr3, '(f10.7)') y_lo
            write (dbgstr4, '(f10.7)') y_hi
            call draw_text(dbgstr1//c_null_char, 5, 0, 3, color_type(255,255,255,255))
            call draw_text(dbgstr2//c_null_char, 5, 10, 3, color_type(255,255,255,255))
            call draw_text(dbgstr3//c_null_char, 5, 20, 3, color_type(255,255,255,255))
            call draw_text(dbgstr4//c_null_char, 5, 30, 3, color_type(255,255,255,255))
        end if
        call end_draw()
    enddo
    call close_win()
end program
