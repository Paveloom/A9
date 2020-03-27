submodule ( ephemeris_do_m ) ephemeris_do_calculate_verbose_s
implicit none

     contains

     ! Процедура для вычисления эфемериды (с дополнительным выводом)
     module procedure ephemeris_do_calculate_verbose

          ! Значение числа sqrt( (1+e) / (1-e) )
          real(RP) :: sqrt_v

          ! Некоторые элементы орбиты (в радианах)
          real(RP) :: i_r ! Наклонение плоскости орбиты к плоскости эклиптики (радианы)
          real(RP) :: small_omega_r ! Угловое расстояние перигелия от восходящего узла (радианы)
          real(RP) :: capital_omega_r ! Гелиоцентрическая долгота восходящего узла (радианы)
          real(RP) :: M_0_r ! Момент прохождения через перигелий (радианы)

          real(RP) :: n ! Средняя угловая скорость
          real(RP) :: M ! Средняя аномалия
          real(RP) :: EA ! Эксцентрическая аномалия
          real(RP) :: EA_prev ! Предыдущее значение эксцентрической аномалии

          real(RP) :: theta ! Истинная аномалия
          real(RP) :: r ! Расстояние

          real(RP) :: u ! Аргумент широты
          real(RP) :: xc, yc, zc ! Координаты в эклиптической системе координат
          real(RP) :: yce, zce ! Координаты в экваториальной системе координат

          ! Вспомогательные переменные для вычисления координат
          real(RP) :: r_cos_u ! Значение r * cos(u)
          real(RP) :: r_sin_u ! Значение r * sin(u)
          real(RP) :: cos_capital_omega ! Значение cos(capital_omega)
          real(RP) :: sin_capital_omega ! Значение sin(capital_omega)
          real(RP) :: cos_i ! Значение cos(i)
          real(RP) :: sin_i ! Значение sin(i)

          real(RP) :: xcg, ycg, zcg !  Геоцентрические координаты
          real(RP) :: ro ! Радиус-вектор в геоцентрических координатах

          ! Вспомогательная переменная при вычислении
          ! прямых восхождений и склонений
          real(RP) :: zcg_sq ! Значение zcg * zcg

          integer(JP) :: j ! Счетчики
          integer(SP) :: stat ! Статусная переменная

          ! Вывод информации о вызове процедуры
          write(*,'(/, 5x, a, /)') 'Вызвана процедура вычисления эфемериды'

          ! Проверка, выделена ли память под массивы во входных данных
          if ( .not. allocated(input%dates) ) call ephemeris_do_log_error('NA_dates')
          if ( .not. allocated(input%X) ) call ephemeris_do_log_error('NA_X')
          if ( .not. allocated(input%Y) ) call ephemeris_do_log_error('NA_Y')
          if ( .not. allocated(input%Z) ) call ephemeris_do_log_error('NA_Z')

          ! Распаковка входных данных
          associate( N_JP => input%N, &
                   & dates => input%dates, &
                   & date => input%date, &
                   & a => input%a, &
                   & e => input%e, &
                   & i => input%i, &
                   & small_omega => input%small_omega, &
                   & capital_omega => input%capital_omega, &
                   & M_0 => input%M_0, &
                   & X => input%X, &
                   & Y => input%Y, &
                   & Z => input%Z )

          ! Проверка, выделена ли память под массив дат
          if ( allocated(result%dates) ) then

               if ( .not. (size(result%dates, kind=JP) .eq. N_JP) ) then

                    ! Освобождение памяти из-под массива дат
                    deallocate( result%dates, stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WD_dates')

                    ! Выделение памяти под массив дат
                    allocate( result%dates(1_JP:N_JP), stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_dates')

               endif

          else

               ! Выделение памяти под массив дат
               allocate( result%dates(1_JP:N_JP), stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_dates')

          endif

          ! Проверка, выделена ли память под массив значений прямых восхождений
          if ( allocated(result%alpha) ) then

               if ( .not. (size(result%alpha, kind=JP) .eq. N_JP) ) then

                    ! Освобождение памяти из-под массива значений прямых восхождений
                    deallocate( result%alpha, stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WD_alpha')

                    ! Выделение памяти под массив значений прямых восхождений
                    allocate( result%alpha(1_JP:N_JP), stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_alpha')

               endif

          else

               ! Выделение памяти под массив значений прямых восхождений
               allocate( result%alpha(1_JP:N_JP), stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_alpha')

          endif

          ! Проверка, выделена ли память под массив значений склонений
          if ( allocated(result%delta) ) then

               if ( .not. (size(result%delta, kind=JP) .eq. N_JP) ) then

                    ! Освобождение памяти из-под массива значений склонений
                    deallocate( result%delta, stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WD_alpha')

                    ! Выделение памяти под массив значений склонений
                    allocate( result%delta(1_JP:N_JP), stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_alpha')

               endif

          else

               ! Выделение памяти под массив значений склонений
               allocate( result%delta(1_JP:N_JP), stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_alpha')

          endif

          ! Вычисление числа sqrt( (1+e) / (1-e) )
          sqrt_v = sqrt( ( 1._RP + e )/( 1._RP - e ) )

          ! Перевод некоторых элементов орбиты
          ! из градусной меры в радианную
          i_R = pi_180 * i
          small_omega_r = pi_180 * small_omega
          capital_omega_r = pi_180 * capital_omega
          M_0_r = pi_180 * M_0

          ! Вычисление некоторых вспомогательных
          ! переменных для вычисления координат
          cos_capital_omega = cos(capital_omega_r)
          sin_capital_omega = sin(capital_omega_r)
          cos_i = cos(i_r)
          sin_i = sin(i_r)

          ! Вычисление средней угловой скорости
          n = chi * a ** (-3._RP / 2._RP)

          ! Вывод полученного значения
          write(*,'(5x, a, /, 4x, '//RF//', /)') 'Значение средней угловой скорости:', n

          do j = 1_JP, N_JP

               ! Вывод разделителя
               write(*,'(5x, a, /)') '>>'

               ! Вывод момента времени
               write(*,'(5x, a, /, 4x, '//RF//', /)') 'Момент времени:', input%dates(j)

               ! [ Определение орбитальных координат ]

               ! Вычисление средней аномалии
               M = M_0_r + n * (dates(j) - date)

               ! Вывод полученного значения
               write(*,'(5x, a, /, 4x, '//RF//', /)') 'Средняя аномалия:', M

               ! Определение начального значения эксцентрической аномалии
               EA = M
               EA_prev = M + 2._RP * eps_K

               ! Вычисление эксцентрической аномалии
               do while ( abs(EA - EA_prev) .gt. eps_K )

                    EA_prev = EA
                    EA = M + e * sin(EA_prev)

               enddo

               ! Вывод полученного значения
               write(*,'(5x, a, /, 4x, '//RF//', /)') 'Эксцентрическая аномалия:', M

               ! Вычисление истинной аномалии и расстояния
               theta = 2._RP * atan(sqrt_v * tan(EA / 2._RP))
               r = a * (1._RP - e * e) / (1._RP + e * cos(theta))

               ! Вывод полученных значений
               write(*,'(5x, a, /, 4x, '//RF//', /)') 'Истинная аномалия:', theta
               write(*,'(5x, a, /, 4x, '//RF//', /)') 'Расстояние:', r

               ! [ Вычисление гелиоцентрических координат ]

               ! Вычисление аргумента широты
               u = theta + small_omega_r

               ! Вывод полученного значения
               write(*,'(5x, a, /, 4x, '//RF//', /)') 'Аргумент широты:', u

               ! Вычисление некоторых вспомогательных
               ! переменных для вычисления координат
               r_cos_u = r * cos(u)
               r_sin_u = r * sin(u)

               ! Вычисление координат в эклиптической системе координат
               xc = r_cos_u * cos_capital_omega - r_sin_u * sin_capital_omega * cos_i
               yc = r_cos_u * sin_capital_omega + r_sin_u * cos_capital_omega * cos_i
               zc = r_sin_u * sin_i

               ! Вывод полученных значений
               write(*,'(5x, a, /, 4x, 3('//RF//', 3x), /)') 'Координаты в эклиптической системе координат:', xc, yc, zc

               ! Вычисление координат в экваториальной системе координат
               yce = yc * cos_eps - zc * sin_eps
               zce = yc * sin_eps + zc * cos_eps

               ! Вывод полученных значений
               write(*,'(5x, a, /, 4x, 3('//RF//', 3x), /)') 'Координаты в экваториальной системе координат:', xc, yce, zce

               ! [ Вычисление геоцентрических координат ]
               xcg = xc + X(j)
               ycg = yce + Y(j)
               zcg = zce + Z(j)

               ! Вывод полученных значений
               write(*,'(5x, a, /, 4x, 3('//RF//', 3x), /)') 'Геоцентрические координаты:', xcg, ycg, zcg

               ! [ Вычисление прямых восхождений и склонений ]

               ! Вычисление квадрата zcg
               zcg_sq = zcg * zcg

               ! Вычисление значения радиус-вектора
               ro = sqrt(xcg * xcg + ycg * ycg + zcg_sq)

               ! Вычисление прямого восхождения
               result%alpha(j) = atan(xcg / ycg) + pi_2

               ! Вывод полученного значения
               write(*,'(5x, a, /, 5x, a, /)') 'Прямое восхождение:', ephemeris_conversion_DMS(result%alpha(j))

               ! Вычисление склонения
               result%delta(j) = asin(zcg / ro)

               ! Вывод полученного значения
               write(*,'(5x, a, /, 5x, a, /)') 'Склонение:', ephemeris_conversion_DD(result%delta(j))

          enddo

          ! [ Сохранение дат в результате ]
          result%dates(:) = dates(:)

          end associate

     end procedure ephemeris_do_calculate_verbose

end submodule ephemeris_do_calculate_verbose_s