cd ..\apps\%1

docker build -t brecheisen/%1-intel .
docker system prune -f
docker images