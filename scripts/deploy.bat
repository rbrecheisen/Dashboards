@echo off

set /p PASSWORD=<C:\\Users\\r.brecheisen\\dockerhub.txt

docker logout
docker login --username brecheisen --password "%PASSWORD%"

docker push brecheisen/%1-intel:latest