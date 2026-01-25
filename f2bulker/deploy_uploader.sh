#!/bin/bash

REMOTE="serge@cloud-1"
DEST="/opt/opensearch"

echo "📦 Deploying minimal uploader set to $REMOTE..."

# 1. Создаем структуру папок на удаленке
ssh $REMOTE "mkdir -p $DEST/scripts"

# 2. Копируем только скрипт и конфиг
scp scripts/bulk_upload.sh $REMOTE:$DEST/scripts/
scp config.yaml $REMOTE:$DEST/

# 3. Тюним конфиг на VDS (меняем cloud-1 на localhost для скорости и обхода nft)
ssh $REMOTE "sed -i 's/cloud-1/localhost/g' $DEST/config.yaml"

# 4. Проверяем права на исполнение
ssh $REMOTE "chmod +x $DEST/scripts/bulk_upload.sh"

echo "✅ Deployment finished. Now you can run it on VDS."
