run=@export CMHOME=~/Projects/$1; export CMSLACK=$2; export CMNODE=$3; export CODE_LOADING_MODE=interactive; export RELX_REPLACE_OS_VARS=true; rm -rf _build/default/rel/*; rebar3 release; _build/default/rel/cmnode/bin/cmnode console


cm1:
	$(call run,"cmnode","false","cm1")

cm2:
	$(call run,"cmnode","false","cm2")

nkadmin:
	$(call run,"nkadmin","false","nkadmin")

dkv-notifications:
	$(call run,"dkv-notifications","false","dkv")

ci:
	$(call run,"ci","false","ci")

build:
	@docker build -t pedrogutierrez/cmnode:latest .
	
push:
	@docker push pedrogutierrez/cmnode:latest
