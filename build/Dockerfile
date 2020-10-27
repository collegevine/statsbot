FROM haskell:8.8.4-buster

RUN apt-get update && apt-get install curl git libpq-dev -y
RUN git clone https://github.com/CollegeVine/statsbot
RUN cd statsbot && stack install

