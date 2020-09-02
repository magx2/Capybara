FROM openjdk:11 AS COMPILE_CAPYBARA_FILES

WORKDIR /usr/capybara
RUN mkdir out
COPY ./build/libs/Capybara.jar .
COPY ./src/test/resources/capybara test

RUN java -jar Capybara.jar -f ./test -o ./out --debug

FROM python:3.8.5

WORKDIR /usr/src/app
COPY --from=COMPILE_CAPYBARA_FILES  /usr/capybara/out .
CMD ["python", "-m", "compileall", "."]
