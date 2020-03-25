submodule ( ephemeris_do_m ) ephemeris_input_log_error_s
implicit none
     
     contains
     
     ! Процедура для вывода ошибок (входные данные)
     module procedure ephemeris_do_log_error
          
          select case (error_code) ! Проверка кода ошибки

               case ('WA_dates') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Не удалось выделить память под массив дат&
                                                                                 & для объекта типа result_type.'
                    stop

               case ('WA_alpha') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Не удалось выделить память под массив значений прямых восхождений&
                                                                                 & для объекта типа result_type.'
                    stop

               case ('WA_delta') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Не удалось выделить память под массив значений склонений&
                                                                                 & для объекта типа result_type.'
                    stop

               case ('WD_dates') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Не удалось освободить память из-под&
                                                                                 & массива дат объекта&
                                                                                 & типа result_type.'

               case ('WD_alpha') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Не удалось освободить память из-под&
                                                                                 & массива значений прямых восхождений&
                                                                                 & типа result_type.'

               case ('WD_delta') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Не удалось освободить память из-под&
                                                                                 & массива значений склонений&
                                                                                 & типа result_type.'

               case ('NA_dates') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Массив дат не был размещен.&
                                                                                 & Выполнение процедуры невозможно.'
                    stop

               case ('NA_X') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Массив значений координаты X Солнца не был размещен.&
                                                                                 & Выполнение процедуры невозможно.'
                    stop

               case ('NA_Y') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Массив значений координаты Y Солнца не был размещен.&
                                                                                 & Выполнение процедуры невозможно.'
                    stop

               case ('NA_Z') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Массив значений координаты Z Солнца не был размещен.&
                                                                                 & Выполнение процедуры невозможно.'
                    stop

               case default

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_do_log_error:', 'Неизвестный код ошибки ('//error_code//').'

          end select
          
     end procedure ephemeris_do_log_error
     
end submodule ephemeris_input_log_error_s