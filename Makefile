
     ## Это шаблон* make-файла для публикации кода на GitHub.

     ## * Изменен для данного проекта.

     ## Репозиторий на GitHub: https://github.com/Paveloom/B1
     ## Документация: https://www.notion.so/paveloom/B1-fefcaf42ddf541d4b11cfcab63c2f018

     ## Версия релиза: 2.1.3
     ## Версия документации: 2.1.0

     ## Автор: Павел Соболев (http://paveloom.tk)

     ## Для корректного отображения содержимого
     ## символ табуляции должен визуально иметь
     ## ту же длину, что и пять пробелов.

     # Настройки make-файла

     ## Имя координатора
     make_name := make

     ## Указание оболочки
     SHELL := /bin/bash

     ## Указание make-файлу выполнять все правила в одном вызове оболочки
     .ONESHELL : 

     ## Заглушка на вывод сообщений указанными правилами
     ## (без указания имён подавляет вывод со стороны make-файла у всех правил)

     .SILENT :

     ## Правила-псевдоцели
     .PHONY : git, final, new, del, git-am, archive

     ## Правило, выполняющееся при вызове координатора без аргументов
     ALL : git



     # Блок правил для разработки и публикации кода на GitHub

     ## Имя пользователя на GitHub
     username := Paveloom

	## Имя ветки изменений
     FEATURE_BRANCH := feature

     ## Правило для создания и публикации коммита

     git :
 
	      # Определение текущей ветки
	      CURRENT_BRANCH=$$(git status | head -n 1 | cut -d " " -f 3)

	      # Проверка текущей ветки
	      if [ "$$CURRENT_BRANCH" = "${FEATURE_BRANCH}" ]; then

	           # Определение последнего тега
	           LAST_TAG=$$(git describe --tag)

	           # Проверка наличия тега у предыдущего коммита
	           if echo $$LAST_TAG | grep -qv "-"; then

	                # Определение номера сгенерированного ранее тега
	                CURRENT_NUMBER=$$(echo $$LAST_TAG | grep -o "_[0-9]\+" | sed 's/_//')

	                # Проверка наличия сгенерированного ранее тега
	                if echo $$LAST_TAG | grep -q "_"; then

	                     # Прибавление к текущему номеру единицы
	                     NEXT_NUMBER=$$(( $$CURRENT_NUMBER + 1 ))

	                     # Формирование нового тега
	                     NEXT_TAG=$$(echo $$LAST_TAG | sed "s/_$$CURRENT_NUMBER/_$$NEXT_NUMBER/")

	                     git add -A
	                     git commit -e
                     
	                     # Проверка, был ли создан коммит
	                     if [ $$? -eq 0 ]; then

	                          git tag -a $$NEXT_TAG -m "$$NEXT_TAG"
	                          git tag -d $$LAST_TAG
	                          git push origin :$$LAST_TAG
	                          git push --follow-tags

	                     fi

	                else

	                     # Формирование нового тега
	                     NEXT_TAG=$$(echo "$$LAST_TAG _${FEATURE_BRANCH}_1" | sed "s/\ //")

	                     git add -A
	                     git commit -e

	                     # Проверка, был ли создан коммит
	                     if [ $$? -eq 0 ]; then

	                          git tag -a $$NEXT_TAG -m "$$NEXT_TAG"
	                          git push --follow-tags

	                     fi

	                fi

	           else

	                git add -A
	                git commit -e

	                # Проверка, был ли создан коммит
	                if [ $$? -eq 0 ]; then
	                     git push
	                fi
  
	           fi

	      else

	           git add -A
	           git commit -e

	           # Проверка, был ли создан коммит
	           if [ $$? -eq 0 ]; then
	                git push
	           fi

	      fi

     # Правило для удаления последнего тега
     # на ветке изменений локально и удаленно

     final : 

	        # Определение текущей ветки
	        CURRENT_BRANCH=$$(git status | head -n 1 | cut -d " " -f 3)

	        # Проверка текущей ветки
	        if [ "$$CURRENT_BRANCH" = "${FEATURE_BRANCH}" ]; then

	             # Определение последнего тега
	             LAST_TAG=$$(git describe --tag)

	             # Удаление последнего тега удаленно
	             git push origin :$$LAST_TAG

	             # Удаление последнего тега локально
	             git tag -d $$LAST_TAG

	        fi

     ## Правило для создания ветки изменений

     new :
	      git checkout -q master
	      git checkout -b ${FEATURE_BRANCH}
	      git push -u origin ${FEATURE_BRANCH}

	## Правило для удаления текущей ветки изменений локально

     del :
	      git checkout -q master
	      git branch -D ${FEATURE_BRANCH}

     ## Правило для обновления последнего коммита до текущего состояния локального репозитория

     git-am :

	         # Определение последнего тега
	         LAST_TAG=$$(git describe --tag)

	         git add -A
	         git commit --amend

	         # Проверка, был ли создан коммит
	         if [ $$? -eq 0 ]; then

	              # Удаление последнего тега локально
	              git tag -d $$LAST_TAG

	              # Создание последнего тега локально
	              git tag -a $$LAST_TAG -m "$$LAST_TAG"

	              # Удаление последнего тега удаленно
	              git push origin :$$LAST_TAG

	              # Пуш
	              git push --follow-tags --force-with-lease

	         fi


     # Правило для создания архивов

     archive :
	          find H1.\ Вычисление\ эфемериды/ -path '*/.*' -prune -o -type f -print | zip Архивы/H1.zip -FS -q -@
