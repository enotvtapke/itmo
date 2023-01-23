@echo off
set classpath=..\..\..\..\..
call %java_home%\bin\javac Server.java Client.java BankWebServer.java
rem call %java_home%\bin\rmic -d %classpath% examples.rmi.RemoteAccount examples.rmi.RemoteBank
