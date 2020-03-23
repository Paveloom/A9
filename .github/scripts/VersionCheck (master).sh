#!/bin/bash

# Вывод заголовка скрипта
printf "\nЗапущен скрипт, проверяющий, отличается ли текущая версия релиза от предыдущей.\n\n"

# Определение имени ветки с изменениями
FEATURE_BRANCH_NAME="feature"

# Переход на ветку master
git checkout -q master

# Сохранение тега последнего коммита на master в переменную
MASTER_TAG="$(git describe --tags master)"

printf "Проверка, совпадает ли текущий тег с мастер тегом...\n\n"

# Получение текущего тега для H1
H1_CURRENT_TAG="$(grep -o "H1\-v.*\-informational" README.md | grep -o "\-.*\-" | sed 's/-//g')"

# Получение текущего тега для H2
H2_CURRENT_TAG="$(grep -o "H2\-v.*\-informational" README.md | grep -o "\-.*\-" | sed 's/-//g')"

printf "Тег на master:\n"
echo $MASTER_TAG

if grep -q "H1" "$CURRENT_TAG"; then

     printf "\nТекущий тег из README.md для H1:\n"
     echo $H1_CURRENT_TAG

     # Проверка, отличается ли тег на master от текущего тега
     if [ ! $H1_CURRENT_TAG == $MASTER_TAG ]; then

          printf "\nТекущий тег для H1 и тег на master НЕ совпадают.Обновите"
          printf "\nтег на master в соответствии с текущим тегом.\n\n"

          exit 1

     fi

elif grep -q "H2" "$CURRENT_TAG"; then

     printf "\nТекущий тег из README.md для H2:\n"
     echo $H2_CURRENT_TAG

     # Проверка, отличается ли тег на master от текущего тега
     if [ ! $H2_CURRENT_TAG == $MASTER_TAG ]; then

          printf "\nТекущий тег для H2 и тег на master НЕ совпадают.Обновите"
          printf "\nтег на master в соответствии с текущим тегом.\n\n"

          exit 1

     fi

else

     printf "\nТекущий тег не содержит префикса H1 / H2.\n\n"
     exit 1

fi

printf "\nВсё в порядке.\n"