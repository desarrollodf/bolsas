@echo off

cd /d "Y:\Periodistas\DF Datos\apps_shiny\bolsas"

git pull --rebase origin main

python actualizar_datos.py
if errorlevel 1 exit /b 1

git add datos.xlsx

git diff --cached --quiet
if %errorlevel%==0 exit /b 0

git commit -m "Actualizar datos Bloomberg"

git push origin main