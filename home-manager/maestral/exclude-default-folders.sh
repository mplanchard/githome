#!/usr/bin/env bash

folders="
/camera uploads
/dad backup
/family room
/jeannie usb backup
/nautilus
/vid
"

IFS=$'\n'
for folder in $folders; do
    maestral excluded add "$folder"
done
