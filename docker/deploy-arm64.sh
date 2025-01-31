PASSWORD=$(cat $HOME/dockerhubpassword.txt)

docker logout
docker login --username brecheisen --password "$PASSWORD"

docker push brecheisen/$1-arm64:latest
