submodule ( preorb_result_m ) preorb_result_log_error_s
implicit none

     contains

     ! Процедура для вывода ошибок (результат)
     module procedure preorb_result_log_error

          select case (error_code) ! Проверка кода ошибки

               case ('WO')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_result_log_error:', 'Не удалось получить доступ к файлу '//file//'&
                                                                                  & для записи.'
                    stop

               case ('WС')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_result_log_error:', 'Не удалось закрыть файл '//file//'&
                                                                                  & после записи.'
                    stop

               case default

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_result_log_error:', 'Неизвестный код ошибки ('//error_code//').'

          end select

     end procedure preorb_result_log_error

end submodule preorb_result_log_error_s