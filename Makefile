console:
	@export CMNODE_HOME=~/Projects/cmnode; rebar3 release; _build/default/rel/cmnode/bin/cmnode console
