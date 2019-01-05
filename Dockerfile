FROM erlang:21
MAINTAINER Pedro Gutiérrez <pedrogutierrez@mac.com>
RUN apt-get update; apt-get install -y libgd-dev libwebp-dev inotify-tools vim tree
RUN mkdir -p /opt/cmnode/apps; mkdir ~/.ssh; ssh-keyscan github.com >> ~/.ssh/known_hosts
ENV CMHOME /opt/cmnode
ENV CODE_LOADING_MODE interactive
WORKDIR /opt/cmnode
ADD rebar.config .
ADD config config
RUN mkdir etc
RUN rebar3 release
CMD ["/opt/cmnode/_build/default/rel/cmnode/bin/cmnode", "console"]
