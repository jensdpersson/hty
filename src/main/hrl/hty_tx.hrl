

-record(tx, {
		 proto="HTTP/1.1",
		 method='GET',
		 path={[], []},
		 status={200, "OK"},
		 reqh=[],
		 buffered=[],
		 ondata=fun(_Data, _State) -> {ok, noop_state} end,
		 ondata_state=q0,
		 rsph=[],
		 outs=[],
		 loggedin= <<"guest">>,
		 realm=hty_empty_realm,
		 attributes={},
		 queryparams=[]}).