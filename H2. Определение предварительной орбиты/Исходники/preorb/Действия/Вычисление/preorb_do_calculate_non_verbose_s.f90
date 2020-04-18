submodule ( preorb_do_m ) preorb_do_calculate_non_verbose_s
implicit none

     contains

     ! Процедура для определения параметров предварительной
     ! орбиты по трем наблюдениям (без дополнительного вывода)
     module procedure preorb_do_calculate_non_verbose

          real(RP) :: tau_1, tau_2 ! Разницы между средним моментом и другими моментами
          real(RP) :: tau ! Сумма значений tau_1 и tau_2

          ! Вспомогательные переменные
          real(RP) :: n_1_0, n_2_0
          real(RP) :: c_0, c_1, c_2

          ! Значения прямых восхождений и склонений в радианах
          real(RP), dimension(3) :: alpha_r
          real(RP), dimension(3) :: delta_r

          ! Вспомогательный массив для хранения cos(delta_r)
          real(RP), dimension(3) :: cos_delta_r

          ! Направляющие косинусы
          real(RP), dimension(3) :: lambda
          real(RP), dimension(3) :: mu
          real(RP), dimension(3) :: nu

          ! Вспомогательные переменные
          real(RP) :: lambda_13, mu_13, nu_13
          real(RP), dimension(3) :: U
          real(RP) :: D, Dr
          real(RP) :: P, Q, C
          real(RP) :: Rsq

          ! Геоцентрические и гелиоцентрические расстояния
          real(RP), dimension(3) :: ro, r

          ! Вспомогательные переменные
          real(RP) :: ro_prev
          real(RP) :: r_m3
          real(RP) :: n_1, n_2

          ! Коэффициенты для поиска геоцентрического
          ! и гелиоцентрического расстояний первого
          ! и третьего моментов наблюдений
          real(RP) ko_a_1, ko_b_1, ko_c_1
          real(RP) ko_a_2, ko_b_2, ko_c_2
          real(RP) ko_a, ko_b, ko_c
          real(RP) ko_d_1, ko_d_2, ko_d_3

          ! Вектора направлений и их нормы
          real(RP), dimension(3) :: r_1, r_2, r_3
          real(RP) :: r_1_norm, r_2_norm, r_3_norm
          real(RP) :: r_12_norm, r_13_norm

          ! Вспомогательные переменные
          real(RP) :: nc_0, nc_1, nc_3
          reaL(RP) :: p
          real(RP) :: sin_2f, cos_2f
          real(RP) :: pr_1, pr_3
          real(RP) :: e_sin_theta
          real(RP) :: ee

          ! Истинные аномалии на первый и третий моменты времени
          real(RP) :: theta_1

          ! Вспомогательная переменная
          real(RP) :: beta

          ! Эксцентрическая аномалия на момент первого наблюдения
          real(RP) :: E_1

          ! Значения sin(eps) и cos(eps)
          real(RP) sin_eps, cos_eps

          ! Вспомогательные вектора
          real(RP), dimension(3) :: nr_1, nr_3
          real(RP), dimension(3) :: l

          ! Вспомогательная переменная
          real(RP) :: sin_capital_omega

          ! Аргумент широты
          real(RP) :: u_arg

          ! Проверка, была ли выделена память под входные данные
          if ( .not. allocated( input%dates ) ) call preorb_do_log_error(input, 'NA_dates')
          if ( .not. allocated( input%alpha ) ) call preorb_do_log_error(input, 'NA_alpha')
          if ( .not. allocated( input%delta ) ) call preorb_do_log_error(input, 'NA_delta')
          if ( .not. allocated( input%X ) ) call preorb_do_log_error(input, 'NA_X')
          if ( .not. allocated( input%Y ) ) call preorb_do_log_error(input, 'NA_Y')
          if ( .not. allocated( input%Z ) ) call preorb_do_log_error(input, 'NA_Z')

          associate( dates => input%dates,                  &
                   & alpha => input%alpha,                  &
                   & delta => input%delta,                  &
                   & X => input%X,                          &
                   & Y => input%Y,                          &
                   & Z => input%Z,                          &
                   & a => result%a,                         &
                   & e => result%e,                         &
                   & i => result%i,                         &
                   & small_omega => result%small_omega,     &
                   & capital_omega => result%capital_omega, &
                   & M_0 => result%M_0                      )

          ! Вычисление tau_1 и tau_2
          tau_1 = dates(2) - dates(1)
          tau_2 = dates(3) - dates(2)

          ! Вычисление tau
          tau = tau_1 + tau_2

          ! Вычисление n_1_0 и n_2_0
          n_1_0 = tau_1 / tau
          n_2_0 = tau_2 / tau

          ! Вычисление c_1 и c_2
          c_0 = 1._RP / 6._RP * chi_sq * tau_1 * tau_2
          c_1 = c_0 * (1._RP + n_1_0)
          c_2 = c_0 * (1._RP + n_2_0)

          ! Перевод прямых восхождений и склонений в радианы
          alpha_r = alpha * pi_12
          delta_r = delta * pi_180

          ! Вычисление значений направляющих косинусов
          cos_delta_r = cos(delta_r)
          lambda = cos_delta_r * cos(alpha_r)
          mu = cos_delta_r * sin(alpha_r)
          nu = sin(delta_r)

          ! Вычисление lambda_13, mu_13, nu_13
          lambda_13 = mu(1) * nu(3) - mu(3) * nu(1)
          mu_13 = nu(1) * lambda(3) - nu(3) * lambda(1)
          nu_13 = lambda(1) * mu(3) - lambda(3) * mu(1)

          ! Вычисление U
          U = X * lambda_13 + Y * mu_13 + Z * nu_13

          ! Вычисление Dr
          D = lambda(2) * lambda_13 + mu(2) * mu_13 + nu(2) * nu_13
          Dr = 1._RP / D

          ! Вычисление P, Q, C
          P = Dr * ( U(2) - n_1_0 * U(1) - n_2_0 * U(3) )
          Q = Dr * ( c_1 * U(1) + c_2 * U(3) )
          C = - ( lambda(2) * X(2) + mu(2) * Y(2) + nu(2) * Z(2) )

          ! Вычисление R^2
          Rsq = X(2) * X(2) + Y(2) * Y(2) + Z(2) * Z(2)

          ! Вычисление геоцентрического и гелиоцентрического
          ! расстояний для среднего момента времени
          ro(2) = P
          ro_prev = P - 2._RP * eps_e

          do while ( abs( ro(2) - ro_prev ) .ge. eps_e )

               ! Сохранение предыдущего значения ro
               ro_prev = ro(2)

               ! Вычисление значения r
               r(2) = sqrt( ro(2) * ro(2) + 2._RP * C * ro(2) + Rsq )

               ! Вычисление значения ro
               ro(2) = ro_prev - ( ro_prev - P + Q * r(2) ** (-3._RP) ) / ( 1._RP - 3._RP * Q * ( ro_prev + C ) * r(2) ** (-5._RP) )

          enddo

          ! Вычисление значения r
          r(2) = sqrt( ro(2) * ro(2) + 2._RP * C * ro(2) + Rsq )

          ! Вычисление n_1 и n_2
          r_m3 = r(2) ** (-3._RP)
          n_1 = n_1_0 + c_1 * r_m3
          n_2 = n_2_0 + c_2 * r_m3

          ! Вычисление коэффициентов для поиска
          ! геоцентрического и гелиоцентрического
          ! расстояний первого и третьего
          ! моментов наблюдений

          ko_a_1 = n_1 * lambda(1)
          ko_b_1 = n_1 * mu(1)
          ko_c_1 = n_1 * nu(1)

          ko_a_2 = n_2 * lambda(3)
          ko_b_2 = n_2 * mu(3)
          ko_c_2 = n_2 * nu(3)

          ko_a = lambda(2) * ro(2)
          ko_b = mu(2) * ro(2)
          ko_c = nu(2) * ro(2)

          ko_d_1 = n_1 * X(1) - X(2) + n_2 * X(3)
          ko_d_2 = n_1 * Y(1) - Y(2) + n_2 * Y(3)
          ko_d_3 = n_1 * Z(1) - Z(2) + n_2 * Z(3)

          ! Вычисление геоцентрического и гелиоцентрического
          ! расстояний для первого и третьего моментов времени
          ro(3) = ( ko_b_1 * ( ko_d_1 + ko_a ) - ko_a_1 * ( ko_d_2 + ko_b ) ) &
              & / ( ko_a_2 * ko_b_1 - ko_a_1 * ko_b_2 )

          ro(1) = ( ko_d_3 - ko_c_2 * ro(3) + ko_c ) / ko_c_1

          ! Вычисление гелиоцентрических координат для наблюдений

          r_1(1) = ro(1) * lambda(1) - X(1)
          r_1(2) = ro(1) * mu(1) - Y(1)
          r_1(3) = ro(1) * nu(1) - Z(1)

          r_2(1) = ro(2) * lambda(2) - X(2)
          r_2(2) = ro(2) * mu(2) - Y(2)
          r_2(3) = ro(2) * nu(2) - Z(2)

          r_3(1) = ro(3) * lambda(3) - X(3)
          r_3(2) = ro(3) * mu(3) - Y(3)
          r_3(3) = ro(3) * nu(3) - Z(3)

          ! Вычисление nc_1 и nc_3
          nc_0 = preorb_calc_norm_cross(r_1, r_3)
          nc_1 = preorb_calc_norm_cross(r_2, r_3) / nc_0
          nc_3 = preorb_calc_norm_cross(r_1, r_2) / nc_0

          ! Вычисление норм направлений
          r_1_norm = norm2(r_1)
          r_2_norm = norm2(r_2)
          r_3_norm = norm2(r_3)

          ! Вычисление r12_norm и r13_norm
          r_12_norm = r_1_norm * r_2_norm
          r_13_norm = r_1_norm * r_3_norm

          ! Вычисление p
          p = ( nc_1 * r_1_norm + nc_3 * r_3_norm - r_2_norm ) / ( nc_1 + nc_3 - 1._RP )

          ! Вычисление sin(2f) и cos(2f)
          sin_2f = nc_0 / r_13_norm
          cos_2f = sqrt(1._RP - sin_2f * sin_2f)

          ! Вычисление pr_1, pr_3
          pr_1 = p / r_1_norm - 1._RP
          pr_3 = 1._RP - p / r_3_norm

          ! Вычисление e_sin_theta
          e_sin_theta = ( pr_1 * cos_2f + pr_3 ) / sin_2f

          ! Вычисление эксцентриситета
          e = sqrt(e_sin_theta * e_sin_theta + pr_1 * pr_1)

          ! Вычисление квадрата эксцентриситета
          ee = e * e

          ! Вычисление большой полуоси
          a = p / ( 1._RP - ee )

          ! Вычисление истинной аномалии на первый момент времени
          theta_1 = atan2(e_sin_theta, pr_1)

          ! Вычисление beta
          beta = ( 1._RP - sqrt( 1._RP - ee ) ) / e

          ! Вычисление эксцентрической аномалии
          ! на момент первого наблюдения
          E_1 = theta_1 - 2._RP * atan2( beta * sin(theta_1), 1._RP + beta * cos(theta_1) )

          ! Вычисление средней аномалии на
          ! момент первого наблюдения
          M_0 = E_1 - e * sin(E_1)

          ! Вычисление значений sin(eps) и cos(eps)
          sin_eps = sin(eps)
          cos_eps = cos(eps)

          ! Вычисление элементов векторов nr_1 и nr_3

          nr_1(1) = r_1(1)
          nr_1(2) = r_1(3) * sin_eps + r_1(2) * cos_eps
          nr_1(3) = r_1(3) * cos_eps - r_1(2) * sin_eps

          nr_3(1) = r_3(1)
          nr_3(2) = r_3(3) * sin_eps + r_3(2) * cos_eps
          nr_3(3) = r_3(3) * cos_eps - r_3(2) * sin_eps

          ! Вычисление вектора l
          call preorb_calc_cross(nr_1, nr_3, l)

          ! Вычисление долготы восходящего узла
          capital_omega = atan2( -l(2), l(1) )

          ! Вычисление sin(capital_omega)
          sin_capital_omega = sin(capital_omega)

          ! Вычисление наклонения плоскости орбиты к плоскости эклиптики
          i = atan2( l(1) / sin_capital_omega, l(3) )

          ! Вычисление аргумента широты
          u_arg = atan2( ( r_2(3) * cos(e) - r_2(2) * sin(e) ) / sin(i), &
                         & r_1(1) * cos(capital_omega) + ( r_1(3) * sin(e) + r_1(2) * cos(e) ) * sin_capital_omega )

          ! Вычисление аргумента перицентра
          small_omega = u_arg - theta_1

          end associate

     end procedure preorb_do_calculate_non_verbose

end submodule preorb_do_calculate_non_verbose_s