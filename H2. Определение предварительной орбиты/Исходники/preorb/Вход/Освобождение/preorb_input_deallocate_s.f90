submodule ( preorb_input_m ) preorb_input_deallocate_s
implicit none

     contains

     ! Процедура для освобождения памяти (входные данные)
     module procedure preorb_input_deallocate

          integer(SP) :: stat ! Статусная переменная

          ! Освобождение памяти из-под массива дат
          if ( allocated(input%dates) ) then

               deallocate( input%dates, stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WD_dates')

          endif

          ! Освобождение памяти из-под массива прямых восхождений
          if ( allocated(input%alpha) ) then

               deallocate( input%alpha, stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WD_alpha')

          endif

          ! Освобождение памяти из-под массива склонений
          if ( allocated(input%delta) ) then

               deallocate( input%delta, stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WD_delta')

          endif

          ! Освобождение памяти из-под массива значений координат X Солнца
          if ( allocated(input%X) ) then

               deallocate( input%X, stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WD_X')

          endif

          ! Освобождение памяти из-под массива значений координат Y Солнца
          if ( allocated(input%Y) ) then

               deallocate( input%Y, stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WD_Y')

          endif

          ! Освобождение памяти из-под массива значений координат Z Солнца
          if ( allocated(input%Z) ) then

               deallocate( input%Z, stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WD_Z')

          endif

     end procedure preorb_input_deallocate

end submodule preorb_input_deallocate_s