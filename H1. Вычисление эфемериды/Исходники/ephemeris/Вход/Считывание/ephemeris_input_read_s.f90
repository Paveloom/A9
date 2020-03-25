submodule ( ephemeris_input_m ) ephemeris_input_read_s
implicit none
     
     contains
     
     ! Процедура для считывания входных данных
     module procedure ephemeris_input_read
          
          integer(SP) :: stat ! Статусная переменная
          integer(UP) :: unit ! Номер дескриптора файла

          ! Открытие файла
          open( newunit = unit, file = file, action = 'read', status = 'old', iostat = stat)
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WO', file)
          
          ! Пропуск строки
          read( unit = unit, fmt = '()' )

          ! Считывание размера массива дат
          read( unit = unit, fmt = *, iostat = stat ) input%N
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_N', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Проверка, выделена ли память под массив дат
          if ( allocated(input%dates) ) then

               if ( .not. (size(input%dates, kind=JP) .eq. input%N) ) then

                    ! Освобождение памяти из-под массива дат
                    deallocate( input%dates, stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WD_dates')

                    ! Выделение памяти под массив дат
                    allocate( input%dates(1_JP:input%N), stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WA_dates')

               endif

          else

               ! Выделение памяти под массив дат
               allocate( input%dates(1_JP:input%N), stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WA_dates')

          endif

          ! Считывание массива дат
          read( unit = unit, fmt = *, iostat = stat ) input%dates
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_dates', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Считывание опорной даты
          read( unit = unit, fmt = *, iostat = stat ) input%date
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_date', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Считывание большой полуоси
          read( unit = unit, fmt = *, iostat = stat ) input%a
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_a', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Считывание эксцентриситета орбиты
          read( unit = unit, fmt = *, iostat = stat ) input%e
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_e', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Считывание наклонения плоскости орбиты к плоскости эклиптики
          read( unit = unit, fmt = *, iostat = stat ) input%i
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_i', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Считывание углового расстояния перигелия от восходящего узла
          read( unit = unit, fmt = *, iostat = stat ) input%small_omega
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_small_omega', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Считывание гелиоцентрической долготы восходящего узла
          read( unit = unit, fmt = *, iostat = stat ) input%capital_omega
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_capital_omega', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Считывание момента прохождения через перигелий
          read( unit = unit, fmt = *, iostat = stat ) input%M_0
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_M_0', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Проверка, выделена ли память под массив значений координаты X Солнца
          if ( allocated(input%X) ) then

               if ( .not. (size(input%X, kind=JP) .eq. input%N) ) then

                    ! Освобождение памяти из-под массива значений координаты X Солнца
                    deallocate( input%X, stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WD_X')

                    ! Выделение памяти под массив значений координаты X Солнца
                    allocate( input%X(1_JP:input%N), stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WA_X')

               endif

          else

               ! Выделение памяти под массив значений координаты X Солнца
               allocate( input%X(1_JP:input%N), stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WA_X')

          endif

          ! Считывание массива значений координаты X Солнца
          read( unit = unit, fmt = *, iostat = stat ) input%X
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_X', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Проверка, выделена ли память под массив значений координаты Y Солнца
          if ( allocated(input%Y) ) then

               if ( .not. (size(input%Y, kind=JP) .eq. input%N) ) then

                    ! Освобождение памяти из-под массива значений координаты Y Солнца
                    deallocate( input%Y, stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WD_Y')

                    ! Выделение памяти под массив значений координаты Y Солнца
                    allocate( input%Y(1_JP:input%N), stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WA_Y')

               endif

          else

               ! Выделение памяти под массив значений координаты Y Солнца
               allocate( input%Y(1_JP:input%N), stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WA_Y')

          endif

          ! Считывание массива значений координаты Y Солнца
          read( unit = unit, fmt = *, iostat = stat ) input%Y
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_Y', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)' )

          ! Проверка, выделена ли память под массив значений координаты Z Солнца
          if ( allocated(input%Z) ) then

               if ( .not. (size(input%Z, kind=JP) .eq. input%N) ) then

                    ! Освобождение памяти из-под массива значений координаты Z Солнца
                    deallocate( input%Z, stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WD_Z')

                    ! Выделение памяти под массив значений координаты Z Солнца
                    allocate( input%Z(1_JP:input%N), stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WA_Z')

               endif

          else

               ! Выделение памяти под массив значений координаты Z Солнца
               allocate( input%Z(1_JP:input%N), stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WA_Z')

          endif

          ! Считывание массива значений координаты Z Солнца
          read( unit = unit, fmt = *, iostat = stat ) input%Z
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WR_Z', file)

          ! Закрытие файла
          close( unit = unit, iostat = stat )
          if ( stat .ne. 0_SP ) call ephemeris_input_log_error('WC', file)
          
     end procedure ephemeris_input_read
     
end submodule ephemeris_input_read_s