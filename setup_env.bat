@echo off
echo Configurando entorno de R...
Rscript -e "if (!require('pacman')) install.packages('pacman')"
echo Entorno listo.
