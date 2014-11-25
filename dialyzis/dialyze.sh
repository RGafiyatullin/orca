#!/bin/bash

CORE_APPS=" \
erts kernel stdlib \
crypto mnesia sasl \
common_test eunit \
"
CORE_PLT="$(dirname $0)/core.plt"

REL_D="$(dirname $0)/.."
REL_PLT="$(dirname $0)/rel.plt"

DEP_D="$(dirname $0)/../deps"

# DIALYZER_WFLAGS="-Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs"
DIALYZER_WFLAGS="-Werror_handling"

function create-core-plt() {
	echo "Creating core plt ($CORE_PLT) [$CORE_APPS] ..."
	dialyzer --statistics --build_plt --output_plt "$CORE_PLT" -o "${CORE_PLT}.log" --apps $CORE_APPS
}
function check-rel-plt() {
	echo "Checking rel plt ($REL_PLT) ..."
	dialyzer $DIALYZER_WFLAGS --plt "$REL_PLT" -r "$REL_D"
}
function create-rel-plt() {
	echo "Creating rel plt ($REL_PLT) ..."
	cp "$CORE_PLT" "$REL_PLT"
	dialyzer --statistics --add_to_plt --plt "$REL_PLT" -o "${REL_PLT}.log" -r "$REL_D" -r "$DEP_D"
}

[ -f $CORE_PLT ] && echo "Core plt exists ($CORE_PLT)" || create-core-plt
[ -f $REL_PLT ] && echo "Rel plt exists ($REL_PLT)" || create-rel-plt
check-rel-plt
