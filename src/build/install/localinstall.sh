rm -rf /opt/hty
mkdir /opt/hty
cp -r target/* /opt/hty/
rm -rf /usr/local/bin/hty
ln -s /opt/hty/bin/hty /usr/local/bin/hty