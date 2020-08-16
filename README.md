# Capybara

## Compile grammar locally

```shell script
java \
  -jar antlr-4.8-complete.jar \
  -Dlanguage=Python3 \
  -o src/main/capybara/grammar \
  Capybara.g4
```