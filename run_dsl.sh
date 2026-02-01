#!/bin/bash

# Проверяем аргументы на наличие флага verbose
VERBOSE_FLAG=""
for arg in "$@"
do
    if [ "$arg" == "-v" ] || [ "$arg" == "--verbose" ]; then
        VERBOSE_FLAG="-Dsun.java.command=--verbose"
    fi
done

echo "🚀 Starting DSL Server..."
# Мы используем хитрость с sun.java.command или просто передадим аргументы в JAR
# Самый простой способ для нашего кода выше:
java -jar cmd/dsl-scala/dsl-server.jar $@
