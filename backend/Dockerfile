# ============================
# STEP 1: Build the application
# ============================
FROM eclipse-temurin:21-jdk AS builder
WORKDIR /app

# Chỉ cần mvnw + pom.xml (không cần .mvn)
COPY mvnw pom.xml ./

# Cài quyền thực thi cho mvnw
RUN chmod +x mvnw

# Tải dependencies (cache layer)
RUN ./mvnw dependency:go-offline -B

# Copy toàn bộ source (an toàn)
COPY . .

# Build app
RUN ./mvnw clean package -DskipTests -B

# ============================
# STEP 2: Run the application
# ============================
FROM eclipse-temurin:21-jre
WORKDIR /app

COPY --from=builder /app/target/*.jar app.jar

# Ép IPv4 + G1GC
ENV JAVA_TOOL_OPTIONS="-Djava.net.preferIPv4Stack=true -XX:+UseG1GC"

# Railway yêu cầu: dynamic PORT
EXPOSE ${PORT:-8080}

ENTRYPOINT ["java", "-jar", "app.jar", "--server.port=${PORT:-8080}"]