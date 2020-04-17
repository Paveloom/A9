submodule ( preorb_result_m ) preorb_result_write_s
implicit none

     contains

     ! Процедура для записи результата в файл
     module procedure preorb_result_write

          integer(SP) :: stat ! Статусная переменная
          integer(UP) :: unit ! Номер дескриптора файла

          ! Открытие файла
          open( newunit = unit, file = file, action = 'write', status = 'replace', iostat = stat)
          if ( stat .ne. 0_SP ) call result%log('WO', file)

          write( unit, '(a)' ) 'Большая полуось'
          write( unit, '('//RF//')' ) result%a
          write( unit, '()')

          write( unit, '(a)' ) 'Эксцентриситет орбиты'
          write( unit, '('//RF//')' ) result%e
          write( unit, '()')

          if ( present(conversion) ) then

               if ( conversion ) then

                    write( unit, '(a)' ) 'Угол наклона плоскости орбиты к плоскости эклиптики'
                    write( unit, '(*(a, 3x))' ) preorb_conversion_DD(result%i)
                    write( unit, '()')

                    write( unit, '(a)' ) 'Аргумент перицентра'
                    write( unit, '(*(a, 3x))' ) preorb_conversion_DD(result%small_omega)
                    write( unit, '()')

                    write( unit, '(a)' ) 'Долгота восходящего узла'
                    write( unit, '(*(a, 3x))' ) preorb_conversion_DD(result%capital_omega)
                    write( unit, '()')

                    write( unit, '(a)' ) 'Средняя аномалия на момент первого наблюдения'
                    write( unit, '(*(a, 3x))' ) preorb_conversion_DD(result%M_0)

               else

                    write( unit, '(a)' ) 'Угол наклона плоскости орбиты к плоскости эклиптики'
                    write( unit, '('//RF//')' ) result%i
                    write( unit, '()')

                    write( unit, '(a)' ) 'Аргумент перицентра'
                    write( unit, '('//RF//')' ) result%small_omega
                    write( unit, '()')

                    write( unit, '(a)' ) 'Долгота восходящего узла'
                    write( unit, '('//RF//')' ) result%capital_omega
                    write( unit, '()')

                    write( unit, '(a)' ) 'Средняя аномалия на момент первого наблюдения'
                    write( unit, '('//RF//')' ) result%M_0

               endif

          else

               write( unit, '(a)' ) 'Угол наклона плоскости орбиты к плоскости эклиптики'
               write( unit, '('//RF//')' ) result%i
               write( unit, '()')

               write( unit, '(a)' ) 'Аргумент перицентра'
               write( unit, '('//RF//')' ) result%small_omega
               write( unit, '()')

               write( unit, '(a)' ) 'Долгота восходящего узла'
               write( unit, '('//RF//')' ) result%capital_omega
               write( unit, '()')

               write( unit, '(a)' ) 'Средняя аномалия на момент первого наблюдения'
               write( unit, '('//RF//')' ) result%M_0

          endif

          ! Закрытие файла
          close( unit = unit, iostat = stat )
          if ( stat .ne. 0_SP ) call result%log('WC', file)

     end procedure preorb_result_write

end submodule preorb_result_write_s