chcp 437
net share temp=c:\Users\super\OS\OSlite\lab6\shared
::schtasks /create /tn "lab6_copy" /tr c:\Users\super\OS\OSlite\lab6\2.1.bat /sc minute /mo 1
::schtasks /delete /tn lab6_copy
fc /b c:\Users\super\OS\OSlite\lab6\shared\explorer.exe c:\Windows\explorer.exe