FROM elixir:1.10.3

WORKDIR /app
ADD . /app

# Ensure UTF-8 encoding for iex
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN apt-get update && apt-get install -y locales locales-all inotify-tools
RUN rm -rf /var/cache/apt
RUN mix local.hex --force && mix local.rebar --force
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen
RUN locale-gen
