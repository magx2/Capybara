@echo off
setlocal
set "APP_HOME=%~dp0.."
if defined PYTHONPATH (
  set "PYTHONPATH=%APP_HOME%\lib\compiler;%APP_HOME%\lib\native;%APP_HOME%\lib\antlr;%APP_HOME%\lib\runtime;%PYTHONPATH%"
) else (
  set "PYTHONPATH=%APP_HOME%\lib\compiler;%APP_HOME%\lib\native;%APP_HOME%\lib\antlr;%APP_HOME%\lib\runtime"
)
python "%APP_HOME%\launcher\capyc-py.py" %*
exit /b %ERRORLEVEL%
