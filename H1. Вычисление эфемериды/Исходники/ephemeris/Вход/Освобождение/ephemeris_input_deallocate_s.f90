submodule ( ephemeris_input_m ) ephemeris_input_deallocate_s
implicit none

     contains

     ! Процедура для освобождения памяти (входные данные)
     module procedure ephemeris_input_deallocate

          integer(SP) :: stat ! Статусная переменная

          ! Освобождение памяти из-под массива дат
          if ( allocated(input%dates) ) then

               deallocate( input%dates, stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WD_dates')

          endif

          ! Освобождение памяти из-под массива значений координаты X Солнца
          if ( allocated(input%X) ) then

               deallocate( input%X, stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WD_X')

          endif

          ! Освобождение памяти из-под массива значений координаты Y Солнца
          if ( allocated(input%Y) ) then

               deallocate( input%Y, stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WD_Y')

          endif

          ! Освобождение памяти из-под массива значений координаты Z Солнца
          if ( allocated(input%Z) ) then

               deallocate( input%Z, stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WD_Z')

          endif

     end procedure ephemeris_input_deallocate

end submodule ephemeris_input_deallocate_s