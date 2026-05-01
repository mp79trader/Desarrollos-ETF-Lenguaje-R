#!/bin/bash
echo 'Configurando entorno de R...'
# Instalación de pacman para gestión de paquetes
Rscript -e "if (!require('pacman')) install.packages('pacman')"
echo 'Entorno listo.'
