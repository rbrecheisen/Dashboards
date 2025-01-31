cd ../apps/$1
docker run --rm --name $1 --platform linux/amd64 -p 3838:3838 -v $1_data:/home/shiny/.$1 brecheisen/$1-intel
