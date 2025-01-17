cd ..\apps\%1

docker build -t %1 .
docker system prune -f
docker images