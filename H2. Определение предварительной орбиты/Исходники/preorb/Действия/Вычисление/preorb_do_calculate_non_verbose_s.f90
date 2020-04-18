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
          real(RP), dimension(3) :: ro
          real(RP) :: r

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

          ! Вектора гелиоцентрических координат
          ! и нормы первого и третьего
          real(RP), dimension(3) :: r_1, r_2, r_3
          real(RP) :: r_1_norm, r_3_norm
          real(RP) :: r_13_norm

          ! Вспомогательные переменные
          real(RP) :: k, f2, m, l

          ! Отношение площади сектора орбиты к площади треугольника
          real(RP) :: eta, eta_prev

          ! Аргумент функции X
          real(RP) :: x_arg

          ! Значение функции X
          real(RP) :: X_func

          ! Вспомогательные переменные
          real(RP) :: Xm, Xs

          ! Счетчик для итераций
          integer(JP) :: n

          ! Параметр орбиты
          real(RP) :: p

          ! Вспомогательные переменные
          real(RP) :: sin_2f, cos_2f
          real(RP) :: pr_1, pr_3
          real(RP) :: e_sin_theta
          real(RP) :: ee

          ! Истинные аномалии на первый и третий моменты времени
          real(RP) :: theta_1

          ! Значения cos(theta_1) и sin(theta_1)
          real(RP) :: cos_theta_1
          real(RP) :: sin_theta_1

          ! Вспомогательная переменная
          real(RP) :: beta

          ! Эксцентрическая аномалия на момент первого наблюдения
          real(RP) :: E_1

          ! Значения sin(eps) и cos(eps)
          real(RP) sin_eps, cos_eps

          ! Вспомогательная переменная
          real(RP) :: sigma

          ! Вспомогательный вектор r_0 и его норма
          real(RP), dimension(3) :: r_0
          real(RP) :: r_0_norm

          ! Векторные элементы
          real(RP), dimension(3) :: P_vec, Q_vec

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
          tau_1 = chi * ( dates(2) - dates(1) )
          tau_2 = chi * ( dates(3) - dates(2) )

          ! Вычисление tau
          tau = tau_1 + tau_2

          ! Вычисление n_1_0 и n_2_0
          n_1_0 = tau_1 / tau
          n_2_0 = tau_2 / tau

          ! Вычисление c_1 и c_2
          c_0 = 1._RP / 6._RP * tau_1 * tau_2
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
               r = sqrt( ro(2) * ro(2) + 2._RP * C * ro(2) + Rsq )

               ! Вычисление значения ro
               ro(2) = ro_prev - ( ro_prev - P + Q * r ** (-3._RP) ) / ( 1._RP - 3._RP * Q * ( ro_prev + C ) * r ** (-5._RP) )

          enddo

          ! Вычисление значения r
          r = sqrt( ro(2) * ro(2) + 2._RP * C * ro(2) + Rsq )

          ! Вычисление n_1 и n_2
          r_m3 = r ** (-3._RP)
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

          ! Вычисление геоцентрических расстояний для
          ! первого и третьего моментов времени
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

          ! Вычисление норм направлений
          r_1_norm = norm2(r_1)
          r_3_norm = norm2(r_3)
          r_13_norm = r_1_norm * r_3_norm

          ! Вычисление 2f
          f2 = asin(preorb_calc_norm_cross(r_1, r_3) / r_13_norm)

          ! Вычисление k, m и l
          k = 2._RP * sqrt(r_13_norm) * cos(f2 / 2._RP)
          m = tau * tau / k / k / k
          l = ( ( r_1_norm + r_3_norm ) / k - 1._RP ) / 2._RP

          ! Вычисление eta
          eta = 1._RP
          eta_prev = eta + 2._RP * eps_e

          do while ( abs( eta - eta_prev ) .ge. eps_e )

               x_arg = m / eta / eta - l

               X_func = 0._RP
               do n = 0_JP, X_iters

                    X_func = X_func + &
                    & real(preorb_calc_factor_even(2_JP * n + 4_JP) / 2_JP / preorb_calc_factor_odd(2_JP * n + 3_JP), kind = RP) * &
                    & x_arg ** n

               enddo

               eta_prev = eta

               Xm = X_func * m
               Xs = ( 3._RP * sqrt(3._RP) * sqrt(27._RP * Xm * Xm + 4._RP * Xm) + 27._RP * Xm + 2._RP ) ** ( 1._RP / 3._RP ) &
                  & / 2._RP ** (1._RP / 3._RP)

               eta = 1._RP / 3._RP * ( Xs + 1._RP / Xs + 1._RP )

          enddo

          ! Вычисление параметра орбиты
          p = ( eta * r_1_norm * r_3_norm * sin(f2) ) ** 2._RP / tau / tau

          ! Вычисление sin(2f) и cos(2f)
          sin_2f = sin(f2)
          cos_2f = cos(f2)

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

          ! Вычисление косинуса и синуса от theta_1
          cos_theta_1 = cos(theta_1)
          sin_theta_1 = sin(theta_1)

          ! Вычисление beta
          beta = ( 1._RP - sqrt( 1._RP - ee ) ) / e

          ! Вычисление эксцентрической аномалии
          ! на момент первого наблюдения
          E_1 = theta_1 - 2._RP * atan2( beta * sin_theta_1, 1._RP + beta * cos_theta_1 )

          ! Вычисление средней аномалии на
          ! момент первого наблюдения
          M_0 = E_1 - e * sin(E_1)

          ! Вычисление значений sin(eps) и cos(eps)
          sin_eps = sin(eps)
          cos_eps = cos(eps)

          ! Вычисление вспомогательной переменной
          sigma = ( r_1(1) * r_3(1) + r_1(2) * r_3(2) + r_1(3) * r_3(3) ) / r_1_norm / r_1_norm

          ! Вычисление вспомогательного вектора
          r_0(1) = r_3(1) - sigma * r_1(1)
          r_0(2) = r_3(2) - sigma * r_1(2)
          r_0(3) = r_3(3) - sigma * r_1(3)

          ! Вычисление нормы r_0
          r_0_norm = norm2(r_0)

          ! Вычисление P, Q
          P_vec = r_1 / r_1_norm * cos_theta_1 - r_0 / r_0_norm * sin_theta_1
          Q_vec = r_1 / r_1_norm * sin_theta_1 + r_0 / r_0_norm * cos_theta_1

          ! Вычисление аргумента перицентра
          small_omega = atan2(P_vec(3) * cos_eps - P_vec(2) * sin_eps, Q_vec(3) * cos_eps - Q_vec(2) * sin_eps)

          ! Вычисление угла наклона
          i = asin( ( P_vec(3) * cos_eps - P_vec(2) * sin_eps ) / sin(small_omega) )

          ! Вычисление долготы восходящего узла
          capital_omega = asin( ( P_vec(2) * cos(small_omega) - Q_vec(2) * sin(small_omega) ) / cos_eps )

          end associate

     end procedure preorb_do_calculate_non_verbose

end submodule preorb_do_calculate_non_verbose_s