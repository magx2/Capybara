FROM gradle:5.3-jdk11-slim AS BUILD_CAPYBARA_JAR
LABEL maintainer="Martin Grzeslowski <https://github.com/magx2>"

COPY --chown=gradle:gradle . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle build -x test --no-daemon

FROM openjdk:11 AS COMPILE_CAPYBARA_FILES

WORKDIR /usr/capybara
COPY --from=BUILD_CAPYBARA_JAR /home/gradle/src/build/libs/Capybara.jar .
COPY ./src/test/resources/capybara test

#RUN java -jar Capybara.jar ./test
CMD ["java" ,"-jar", "Capybara.jar" ,"./test"]
#CMD ["sleep" , "3h"]
#FROM python:3.8.5
