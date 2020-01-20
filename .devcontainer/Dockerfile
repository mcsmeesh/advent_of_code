FROM haskell:latest

RUN apt-get update &&  apt-get install -y \
    vim

RUN curl https://gist.githubusercontent.com/simonista/8703722/raw/d08f2b4dc10452b97d3ca15386e9eed457a53c61/.vimrc > /root/.vimrc

RUN sed -i s/set\ number/set\ number\ relativenumber/ /root/.vimrc
