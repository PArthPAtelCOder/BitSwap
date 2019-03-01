-- open (nodeid :NodeId, ledger :Ledger)
-- 	TCP connection
-- 	sends open message with Ledger
-- 	if receiver decides not to connect it closes the connection
-- 		(Have blocklist, based on large debt or transmission below zero)(unblock after ignore_cooldown)
-- 	else
-- 		add peer to activePeers, loa ledger from old ledger, if there exists one.
-- 		update
-- 		compare it with received ledger
-- 		if same
-- 			start connection
-- 		else
-- 			reset ledger and send it to sender




-- send_want_list (want_list :WantList)
-- 	(a) upon opening theconnection,
-- 	(b) after a randomized periodic timeout,
-- 	(c) af-ter a change in thewant_listand
-- 	(d) after receiving a newblock.

-- 	upon receiving want_list update it in Peer DS.

-- 	TODO wantListTimestamp ? last


-- send_block (block :Block) -> (complete :Bool)
-- 	sends block, receiver checks hash of received block
-- 	returns confirmation

-- 	after confirmation both nodes updates their correspponding ledgers

-- close (final :Bool)
-- 	True : node is exiting
-- 	False : due to timeout

-- 	After closing both nodes clears state, store ledger for future usage.