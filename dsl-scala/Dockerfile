FROM sbtscala/scala-sbt:eclipse-temurin-17.0.4_1.7.1_3.2.0
WORKDIR /app
COPY . .
# Запускаем сборку
RUN sbt clean assembly
