submodule ( ephemeris_result_m ) ephemeris_result_log_error_s
implicit none
     
     contains
     
     ! Процедура для вывода ошибок (результат)
     module procedure ephemeris_result_log_error
          
          select case (error_code) ! Проверка кода ошибки

               case ('WD_dates') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Не удалось освободить память из-под&
                                                                                     & массива дат объекта&
                                                                                     & типа result_type.'

               case ('WD_X') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Не удалось освободить память из-под&
                                                                                     & массива значений координаты X объекта&
                                                                                     & типа result_type.'

               case ('WD_Y') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Не удалось освободить память из-под&
                                                                                     & массива значений координаты Y объекта&
                                                                                     & типа result_type.'

               case ('WD_Z') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Не удалось освободить память из-под&
                                                                                     & массива значений координаты Z объекта&
                                                                                     & типа result_type.'

               case default

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Неизвестный код ошибки ('//error_code//').'

          end select
          
     end procedure ephemeris_result_log_error
     
end submodule ephemeris_result_log_error_s