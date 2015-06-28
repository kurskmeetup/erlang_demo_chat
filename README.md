# Исходный код, написанный во время live coding session в докладe посвященному Erlang на kurskmeetup 27 июня 2015

Должен быть установлен Erlang. Если установить из пакетов вашей ОС не
получается, можно воспользоваться kerl https://github.com/yrashk/kerl

    git clone https://github.com/kurskmeetup/erlang_demo_chat.git
    cd erlang_demo_chat
    make all
    make run

Приложение ждет клиентов на порту 5000

для запуска в background (как обычно запускаются серверные приложения)

    make deploy


