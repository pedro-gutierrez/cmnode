run=@export CMHOME=~/Projects/$1; export CMSLACK=$2; export CMNODE=$1; export CODE_LOADING_MODE=embedded; export RELX_REPLACE_OS_VARS=true; rebar3 release; _build/default/rel/cmnode/bin/cmnode console


cmnode:
	$(call run,"cmnode","false")

nkadmin:
	$(call run,"nkadmin","false")

ci:
	$(call run,"ci","false")

build:
	@docker build -t pedrogutierrez/cmnode:latest .
	
push:
	@docker push pedrogutierrez/cmnode:latest
