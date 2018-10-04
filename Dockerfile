FROM netcomposer/erlang:19.3.6.1
RUN mkdir -p /opt/cmnode/apps
ENV CMHOME /opt/cmnode
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
