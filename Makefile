console:
	@export CMHOME=~/Projects/cmnode; export CODE_LOADING_MODE=embedded; rebar3 release; _build/default/rel/cmnode/bin/cmnode console

build:
	@docker build -t pedrogutierrez/cmnode:latest .
	
push:
	@docker push pedrogutierrez/cmnode:latest
