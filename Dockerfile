FROM erlang:21
MAINTAINER Pedro Gutiérrez <pedrogutierrez@mac.com>
RUN apt-get update; apt-get install -y libgd-dev libwebp-dev inotify-tools vim tree
RUN mkdir -p /opt/cmnode/apps; mkdir ~/.ssh; ssh-keyscan github.com >> ~/.ssh/known_hosts
ENV CMHOME /opt/cmnode
ENV CODE_LOADING_MODE interactive
WORKDIR /opt/cmnode
ADD rebar.config .
ADD config config
ADD apps apps
ADD etc/apps etc/apps
ADD etc/buckets etc/buckets
ADD etc/crons etc/crons
ADD etc/modules etc/modules
ADD etc/ports etc/ports
ADD etc/queues etc/queues
ADD etc/tasks etc/tasks
ADD etc/templates etc/templates
ADD etc/tests etc/tests
ADD etc/themes etc/themes 
RUN mkdir etc/settings
RUN rebar3 release
CMD ["/opt/cmnode/_build/default/rel/cmnode/bin/cmnode", "console"]
