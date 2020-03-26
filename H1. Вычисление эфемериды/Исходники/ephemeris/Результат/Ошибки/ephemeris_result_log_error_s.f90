submodule ( ephemeris_result_m ) ephemeris_result_log_error_s
implicit none
     
     contains
     
     ! Процедура для вывода ошибок (результат)
     module procedure ephemeris_result_log_error
          
          select case (error_code) ! Проверка кода ошибки

               case ('WO')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Не удалось получить доступ к файлу '//file//'&
                                                                                     & для записи.'
                    stop

               case ('WD_dates') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Не удалось освободить память из-под&
                                                                                     & массива дат объекта&
                                                                                     & типа result_type.'

               case ('WD_alpha') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Не удалось освободить память из-под&
                                                                                     & массива значений прямых восхождений объекта&
                                                                                     & типа result_type.'

               case ('WD_delta') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Не удалось освободить память из-под&
                                                                                     & массива значений склонений объекта&
                                                                                     & типа result_type.'

               case ('NA_dates') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Массив дат не был размещен.&
                                                                                     & Запись невозможна.'
                    stop

               case ('NA_alpha') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Массив значений прямых восхождений не был размещен.&
                                                                                     & Запись невозможна.'
                    stop

               case ('NA_delta') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Массив значений склонений не был размещен.&
                                                                                     & Запись невозможна.'
                    stop

               case ('WС')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Не удалось закрыть файл '//file//'&
                                                                                     & после записи.'
                    stop

               case default

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_result_log_error:', 'Неизвестный код ошибки ('//error_code//').'

          end select
          
     end procedure ephemeris_result_log_error
     
end submodule ephemeris_result_log_error_s