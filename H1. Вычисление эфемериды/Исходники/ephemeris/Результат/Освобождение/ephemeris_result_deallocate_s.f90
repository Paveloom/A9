submodule ( ephemeris_result_m ) ephemeris_result_deallocate_s
implicit none
     
     contains
     
     ! Процедура для освобождения памяти (входные данные)
     module procedure ephemeris_result_deallocate
          
          integer(SP) :: stat ! Статусная переменная

          ! Освобождение памяти из-под массива дат
          if ( allocated(result%dates) ) then
          
               deallocate( result%dates, stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_result_log_error('WD_dates')
               
          endif

          ! Освобождение памяти из-под массива значений прямых восхождений
          if ( allocated(result%alpha) ) then
          
               deallocate( result%alpha, stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_result_log_error('WD_alpha')
               
          endif

          ! Освобождение памяти из-под массива значений склонений
          if ( allocated(result%delta) ) then
          
               deallocate( result%delta, stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_result_log_error('WD_delta')
               
          endif

     end procedure ephemeris_result_deallocate
     
end submodule ephemeris_result_deallocate_s