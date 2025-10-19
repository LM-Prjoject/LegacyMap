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

# Sau đó mới copy source code (để tận dụng cache Docker)
COPY src ./src

# Build ứng dụng (bỏ qua test)
RUN ./mvnw clean package -DskipTests

# ============================
# STEP 2: Run the application
# ============================
FROM eclipse-temurin:21-jre
WORKDIR /app

# Copy file jar đã build từ bước builder
COPY --from=builder /app/target/*.jar app.jar

# ✅ Ép container chỉ dùng IPv4 (fix lỗi DNS)
ENV JAVA_TOOL_OPTIONS="-Djava.net.preferIPv4Stack=true"

# Render sẽ tự dùng cổng 8080, em chỉ cần expose
EXPOSE 8080

# Chạy ứng dụng
ENTRYPOINT ["java", "-jar", "app.jar"]
