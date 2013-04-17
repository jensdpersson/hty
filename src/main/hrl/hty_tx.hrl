

-record(tx, {
		 proto="HTTP/1.1",
		 method='GET',
		 path={[], []},
		 status={200, "OK"},
		 reqh=[],
		 buffered=[],
		 ondata=fun(_Data, _State, Htx) -> {ok, noop_state, Htx} end,
		 ondata_state=q0,
		 rsph=[],
		 outs=[],
		 principal= <<"guest">>,
		 realm=hty_empty_realm,
		 attributes={},
		 queryparams=[],
		 socketreader=no,
		 unread=0}).