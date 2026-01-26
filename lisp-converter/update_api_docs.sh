#!/bin/bash
# Скрипт автоматического обновления API Reference на основе .proto файлов

DOC_FILE="$HOME/projects/ebusta/doc/GRPC_API_REFERENCE.md"
PROTO_DIR="$HOME/projects/ebusta/lisp-converter"
GRPCURL="$HOME/projects/ebusta/lisp-converter/grpcurl"

echo "# GRPC API Reference (Auto-generated)" > $DOC_FILE
echo "Дата обновления: $(date)" >> $DOC_FILE
echo "" >> $DOC_FILE

for proto in $PROTO_DIR/*.proto; do
    filename=$(basename "$proto")
    echo "## Протокол: $filename" >> $DOC_FILE
    echo "\`\`\`bash" >> $DOC_FILE
    # Выводим список сервисов и методов из каждого файла
    $GRPCURL -import-path $PROTO_DIR -proto $proto list >> $DOC_FILE
    echo "\`\`\`" >> $DOC_FILE
    
    echo "### Детальное описание:" >> $DOC_FILE
    echo "\`\`\`proto" >> $DOC_FILE
    # Описываем структуру сообщений внутри файла
    $GRPCURL -import-path $PROTO_DIR -proto $proto describe >> $DOC_FILE
    echo "\`\`\`" >> $DOC_FILE
    echo "---" >> $DOC_FILE
done

echo "✅ Документация в $DOC_FILE обновлена."
