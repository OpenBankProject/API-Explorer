# Build as build stage named "maven"
FROM maven:3-jdk-8 as maven
WORKDIR /usr/src
COPY pom.xml .
COPY src src
#Copy default props file
COPY src/main/resources/props/sample.props.template src/main/resources/props/default.props
RUN mvn -e -B dependency:resolve
RUN mvn -e -B package

# Stage two: openjdk
FROM openjdk:8-jre-alpine

# Add obp user 
RUN adduser -D obp

# Download jetty TODO: Use jetty image
RUN wget https://repo1.maven.org/maven2/org/eclipse/jetty/jetty-distribution/9.4.15.v20190215/jetty-distribution-9.4.15.v20190215.tar.gz
RUN tar xvf jetty-distribution-9.4.15.v20190215.tar.gz

# Copy source from maven build stage
COPY --from=maven /usr/src/target/API_Explorer-1.0.war jetty-distribution-9.4.15.v20190215/webapps/ROOT.war

WORKDIR jetty-distribution-9.4.15.v20190215/
# Switch to the obp user (non root)
USER obp

EXPOSE 8080
# Start jetty
ENTRYPOINT ["java", "-jar", "start.jar"]
