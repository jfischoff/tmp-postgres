apt-get update -q
apt-get install gcc libgmp-dev alex happy curl libpq-dev libcurl4-gnutls-dev libpcre3-dev libffi-dev make python-software-properties vim ctags git tmux ruby ufw fail2ban xz-utils zsh libpq-dev -y -q

echo "-----> Installing Stack"
wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main'|tee /etc/apt/sources.list.d/fpco.list
apt-get update && apt-get install stack -y