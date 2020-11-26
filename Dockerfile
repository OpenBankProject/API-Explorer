# Build as build stage named "maven"
FROM maven:3-jdk-8 as maven
WORKDIR /usr/src
COPY pom.xml .
COPY src src
# Copy default props file
COPY src/main/resources/props/sample.props.template src/main/resources/props/default.props
COPY src/main/resources/container.logback.xml.example src/main/resources/default.logback.xml
RUN mvn -e -B dependency:resolve
RUN mvn -e -B package

FROM jetty

# Copy source from maven build stage
COPY --from=maven /usr/src/target/API_Explorer-1.0.war /var/lib/jetty/webapps/root.war

EXPOSE 8080
