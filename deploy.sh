#!/bin/sh

USER=henrique
HOST=leg.ufpr.br
DIR=public_html/
read -p 'PORTA: ' PORT

rsync -avz -e "ssh -p $PORT" . ${USER}@${HOST}:~/${DIR}/

exit 0