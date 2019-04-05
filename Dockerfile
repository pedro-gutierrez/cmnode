FROM erlang:21.2.3
MAINTAINER Pedro Gutiérrez <pedrogutierrez@mac.com>
RUN apt-get update; apt-get install -y libgd-dev libwebp-dev inotify-tools vim tree
RUN mkdir -p /opt/cmnode/apps; mkdir ~/.ssh; ssh-keyscan github.com >> ~/.ssh/known_hosts
ENV CMHOME /opt/cmnode
RUN rm -rf rebar3; wget https://github.com/erlang/rebar3/releases/download/3.9.1/rebar3; chmod +x rebar3
ENV CODE_LOADING_MODE interactive
ENV RELX_REPLACE_OS_VARS true
ENV CMNODE cmnode
WORKDIR /opt/cmnode
ADD rebar.config .
ADD config config
ADD apps apps
RUN mkdir etc
RUN rebar3 release
CMD ["/opt/cmnode/_build/default/rel/cmnode/bin/cmnode", "console"]
