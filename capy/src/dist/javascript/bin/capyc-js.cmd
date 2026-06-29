@echo off
setlocal
set "APP_HOME=%~dp0.."
if defined NODE_PATH (
  set "NODE_PATH=%APP_HOME%\lib\compiler;%APP_HOME%\lib\antlr;%APP_HOME%\lib\node_modules;%NODE_PATH%"
) else (
  set "NODE_PATH=%APP_HOME%\lib\compiler;%APP_HOME%\lib\antlr;%APP_HOME%\lib\node_modules"
)
node "%APP_HOME%\launcher\capyc-js.js" %*
exit /b %ERRORLEVEL%
