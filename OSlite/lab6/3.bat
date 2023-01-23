chcp 437
sc queryex type=service state=all > services1.txt
::sc stop Dnscache
sleep 1
sc queryex type=service state=all > services2.txt
fc services1.txt services2.txt
::sc start Dnscache