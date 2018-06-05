console:
	@export CMHOME=~/Projects/cmnode; rebar3 release; _build/default/rel/cmnode/bin/cmnode console

foreground:
	@export CMHOME=~/Projects/cmnode; rebar3 release; _build/default/rel/cmnode/bin/cmnode foreground
