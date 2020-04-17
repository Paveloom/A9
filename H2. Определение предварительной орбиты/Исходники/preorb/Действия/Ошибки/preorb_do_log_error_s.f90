submodule ( preorb_do_m ) preorb_input_log_error_s
implicit none

     contains

     ! Процедура для вывода ошибок (действия)
     module procedure preorb_do_log_error

          select case (error_code) ! Проверка кода ошибки

               case ('NA_dates')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_do_log_error:', 'Массив дат не был размещен.&
                                                                              & Выполнение процедуры невозможно.'
                    call input%deallocate()
                    stop

               case ('NA_alpha')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_do_log_error:', 'Массив прямых восхождений не был размещен.&
                                                                              & Выполнение процедуры невозможно.'
                    call input%deallocate()
                    stop

               case ('NA_delta')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_do_log_error:', 'Массив склонений не был размещен.&
                                                                              & Выполнение процедуры невозможно.'
                    call input%deallocate()
                    stop

               case ('NA_X')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_do_log_error:', 'Массив значений координат X Солнца не был размещен.&
                                                                              & Выполнение процедуры невозможно.'
                    call input%deallocate()
                    stop

               case ('NA_Y')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_do_log_error:', 'Массив значений координат Y Солнца не был размещен.&
                                                                              & Выполнение процедуры невозможно.'
                    call input%deallocate()
                    stop

               case ('NA_Z')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_do_log_error:', 'Массив значений координат Z Солнца не был размещен.&
                                                                              & Выполнение процедуры невозможно.'
                    call input%deallocate()
                    stop

               case default

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_do_log_error:', 'Неизвестный код ошибки ('//error_code//').'

          end select

     end procedure preorb_do_log_error

end submodule preorb_input_log_error_s