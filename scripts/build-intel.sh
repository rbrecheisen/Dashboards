cd ../apps/$1

docker build --platform linux/amd64 -t brecheisen/$1-intel .
docker system prune -f
docker images
