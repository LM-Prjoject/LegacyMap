# ============================
# STEP 1: Build the application
# ============================
FROM eclipse-temurin:21-jdk AS builder
WORKDIR /app

# Sao chép Maven Wrapper và pom.xml trước (để cache dependency)
COPY mvnw pom.xml ./
COPY .mvn .mvn

# Cài quyền thực thi cho mvnw
RUN chmod +x mvnw

# Tải trước dependencies để tăng tốc build
RUN ./mvnw dependency:go-offline -B

# DÙNG COPY . . ĐỂ TRÁNH LỖI "src not found"
COPY . .

# Build ứng dụng (bỏ qua test)
RUN ./mvnw clean package -DskipTests -B

# ============================
# STEP 2: Run the application
# ============================
FROM eclipse-temurin:21-jre
WORKDIR /app

# Copy file jar đã build từ bước builder
COPY --from=builder /app/target/*.jar app.jar

# Ép container chỉ dùng IPv4 (fix lỗi DNS)
ENV JAVA_TOOL_OPTIONS="-Djava.net.preferIPv4Stack=true"

# Railway yêu cầu: listen trên $PORT (mặc định 8080)
EXPOSE ${PORT:-8080}

# Chạy ứng dụng với server.port từ env var PORT
ENTRYPOINT ["java", "-jar", "app.jar", "--server.port=${PORT:-8080}"]