submodule ( ephemeris_conversion_m ) ephemeris_conversion_DD_s
implicit none

     contains

     ! Функция для конвертации из радианной меры в градусную
     module procedure ephemeris_conversion_DD

          real(RP) :: value_D ! Число в градусной мере

          real(RP) :: degrees_RP ! Градусы (вещественное)
          integer(IP) :: degrees ! Градусы (целое)
          real(RP) :: minutes_RP ! Угловые минуты (вещественное)
          integer(IP) :: minutes ! Угловые минуты (целое)
          real(RP) :: seconds_RP ! Угловые секунды (вещественное)
          integer(IP) :: seconds ! Угловые секунды (целое)

          real(RP) :: tmp ! Временная переменная

          ! Вспомогательные строки
          character(chars) :: f1, f2, f3, f4

          ! Перевод числа в градусную меру
          value_D = value * 180._RP / pi

          ! Вычисление градусов
          degrees = int(sign(1._RP, value_D), kind=IP) * floor(abs(value_D))
          degrees_RP = real(abs(degrees), kind=RP)

          ! Перевод градусов
          do while (degrees .ge. 360_IP)

               degrees = degrees - 360_IP

          enddo

          do while (degrees .lt. -360_IP)

               degrees = degrees + 360_IP

          enddo

          ! Вычитание градусов и переход к минутам
          tmp = (abs(value_D) - degrees_RP) * 60._RP

          ! Вычисление минут
          minutes = floor(tmp)
          minutes_RP = real(minutes, kind=RP)

          ! Вычитание минут и переход к секундам
          tmp = (tmp - minutes_RP) * 60._RP

          ! Вычисление секунд
          seconds = floor(tmp)
          seconds_RP = real(seconds, kind=RP)

          ! Вычитание секунд
          tmp = tmp - seconds_RP

          ! Запись результатов во внутренние файлы
          write(f1, *) degrees
          write(f2, *) minutes
          write(f3, *) seconds
          write(f4, '('//RF//')') tmp

          f4 = adjustl(f4)
          allocate(out, source = trim(adjustl(f1))//'°'//trim(adjustl(f2))//"'"//trim(adjustl(f3))//'"'//trim(f4(2:)))

     end procedure ephemeris_conversion_DD

end submodule ephemeris_conversion_DD_s