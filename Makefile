run=@export CMHOME=~/Projects/$1; export CMDATA=~/Projects/$1/data/$3; export CMSLACK=$2; export CMNODE="$1$3"; export CODE_LOADING_MODE=interactive; export RELX_REPLACE_OS_VARS=true; _build/default/rel/cmnode/bin/cmnode console


cm1:
	$(call run,"cmnode","false","1")

cm2:
	$(call run,"cmnode","false","2")

cm3:
	$(call run,"cmnode","false","3")

nkadmin:
	$(call run,"nkadmin","false","")

dkv:
	$(call run,"dkv-notifications","false","")

ci:
	$(call run,"ci","false","")

rel:
	@rm -rf _build/default/rel/*; rebar3 release

docker:
	@docker build -t pedrogutierrez/cmnode:latest .
	
push:
	@docker push pedrogutierrez/cmnode:latest
