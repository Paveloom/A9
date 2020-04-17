program main ! Программа, демонстрирующая использование модуля
             ! для определения параметров предварительной орбиты
             ! по трем наблюдениям
use preorb ! Модуль для определения параметров
           ! предварительной орбиты по трем наблюдениям
implicit none

     type( preorb_API ) :: p ! Экземпляр API модуля

     call p%input%read("Файлы/input") ! Считывание входных данных

     call p%calc(verbose = .true.) ! Определение параметров предварительной орбиты

     call p%result%write("Файлы/result", conversion=.true.) ! Запись результата в файл

     call p%deallocate() ! Освобождение памяти

end program main