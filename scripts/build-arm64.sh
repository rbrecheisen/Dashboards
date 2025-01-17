cd ../apps/$1

docker build --platform linux/arm64 -t brecheisen/$1-arm64 .
docker system prune -f
docker images
