#!/bin/bash

echo "Programa para cambiar la configuracion de inicio"
echo "Inicio sin Barra ni escritorio "
sudo cp /home/pi/.config/lxsession/LXDE-pi/autostart2 /home/pi/.config/lxsession/LXDE-pi/autostart
sudo reboot
