FROM mgrzeslowski/capybara:latest AS COMPILE_CAPYBARA
LABEL maintainer="Martin Grzeslowski <https://github.com/magx2>"
COPY integration_test /capybara/src
RUN java \
	-cp /app/resources:/app/classes:/app/libs/* \
	com.magx2.capybara.CapybaraKt \
	-o /capybara/out \
	-f /capybara/src \
	--clear-output \
	--debug

FROM python:3.8.5
WORKDIR /usr/src/app
COPY --from=COMPILE_CAPYBARA /capybara/out .
CMD ["python", "integration_test.py"]
