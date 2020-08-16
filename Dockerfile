FROM openjdk:7 AS grammar_compiler

WORKDIR /usr/src/app
COPY antlr-4.8-complete.jar .
COPY Capybara.g4 .

RUN java \
	-jar \
	antlr-4.8-complete.jar \
	-Dlanguage=Python3 \
	-o out \
	Capybara.g4

FROM python:3.8.5-slim

# Set up and activate virtual env
ENV VIRTUAL_ENV "/venv"
RUN python -m venv $VIRTUAL_ENV
ENV PATH "$VIRTUAL_ENV/bin:$PATH"

WORKDIR /usr/src/app

# Install requirements
COPY src/main/resources/requirements.txt .
RUN python -m pip install -r requirements.txt
RUN rm -f requirements.txt

# Copy gramma
COPY src src
RUN rm -rf src/main/capybara/grammar
COPY --from=grammar_compiler /usr/src/app/out src/main/capybara/grammar

WORKDIR /usr/src/app/src/main/capybara
CMD ["python", "Capybara.py", "../../test/resources/simple_struct.cb"]
#CMD ["sleep", "3h"]