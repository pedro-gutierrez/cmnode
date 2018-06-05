FROM erlang:19.3.6.1
MAINTAINER Pedro Gutiérrez <pedrogutierrez@mac.com>
RUN apt-get update; apt-get install -y libgd-dev libwebp-dev inotify-tools
RUN mkdir /opt/cmnode
WORKDIR /opt/cmnode
ADD . /opt/cmnode
ENV CMNODE_HOME /opt/cmnode
RUN rm -rdf _build/*
RUN rebar3 release
CMD ["_build/default/rel/cmnode/bin/cmnode", "console"]
