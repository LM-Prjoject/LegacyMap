# ============================
# STEP 1: Build
# ============================
FROM eclipse-temurin:21-jdk AS builder
WORKDIR /app

# Copy Maven Wrapper + pom.xml
COPY mvnw pom.xml ./
COPY .mvn .mvn

RUN chmod +x mvnw

# BỎ DÒNG NÀY ĐỂ TRÁNH LỖI
# RUN ./mvnw dependency:go-offline -B

# Copy source
COPY src ./src

# Build app (tự tải dependencies nếu cần)
RUN ./mvnw clean package -DskipTests -B

# ============================
# STEP 2: Run
# ============================
FROM eclipse-temurin:21-jre
WORKDIR /app
COPY --from=builder /app/target/*.jar app.jar

ENV JAVA_TOOL_OPTIONS="-Djava.net.preferIPv4Stack=true"
EXPOSE ${PORT:-8080}
ENTRYPOINT ["java", "-jar", "app.jar", "--server.port=${PORT:-8080}"]