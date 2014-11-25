# Orca - a proper MySQL client for Erlang.

## Features
###Currently supported:

* ####Text protocol
> This is about SELECT, INSERT, DELETE, UPDATE and event LOAD DATA INFILE|OUTFILE statements.
>
> These are supported: the commands are encoded and passed to the database server the responses are decoded and provided as neat erlang terms correctly.

* ####LOAD DATA LOCAL INFILE
> Yes, this is quite a feature. It's still somewhat tricky to use, yet it works.
>
> It's the nearest future when a convenient way of running this kind of query is provided.

* ####Built in connection-pool (with query pipelining)
> Well, frankly speaking it's not a Pool it's a connection manager that dispatches the queries to the connections supervised by it.
> 
> This enables us to use query pipelining: you do not have to wait for a response prior to sending another query.

* ####Ability to use your own very favourite Pool
> Do you happen to like **poolboy**? 
> 
> Does your architecture require a custom resource management approach?
> 
> That is not a problem. You can use **orca_conn** as a single-connection interface.

* ####Packetwise protocol access
> Okay you are looking for a tool that will help you to pump `binlogs` (for some reason).
> 
> `orca_conn_srv` may help you here: it provides packet-level interface to the connection.
>
> I hope you know what you do. 

* ####OTP-compatible
> What if a database access-point would follow that great concept of having a `controlling_processes` and `{active,once}`. 
> 
> Okay, `orca_conn` and `orca` (connection and connection manager respectively) do obey these approaches.
> 
> Also the orca-application itself does not have its own supervision tree. Because most likely you already have one.
> 
> And it's up to you to decide: 
> 
> * what supervisor should look after the connection to the database;
> * what restart strategy to use;
> * and when to give up and shutdown
> 
> Both **orca** and **orca_conn** provide the `start_link/1,2` functions with quite a familiar semantics.
> 


## Getting started

### Make it

	$ git clone https://github.com/RGafiyatullin/orca.git
	$ cd orca
	$ make compile run


### A single connection

	Eshell VX.X.X.X  (abort with ^G)
	1> {ok, Conn} = orca_conn:start_link( <<"mysql://user:passwd@localhost/db_name">> ).
	2> {ok, #orca_rows{ rows = Rows }} = orca_conn:sql( Conn, 
			<<"SELECT * FROM information_schema.processlist P WHERE P.DB = ?">>, [ <<"db_name">> ] ).

### A connection manager

	Eshell VX.X.X.X  (abort with ^G)
	1> {ok, ConnMgr} = orca:start_link( <<"mysql://user:passwd@localhost/"
								"db_name?pool_size=4&min_restart_interval=1500">> ).
	2> {ok, #orca_rows{ rows = Rows }} = orca_conn:sql( Conn, 
		<<"SELECT * FROM information_schema.processlist P WHERE P.DB = ?">>, [ <<"db_name">> ] ).

##  Licence
	The MIT License (MIT)
	
	Copyright (c) 2014 Roman Gafiyatullin
	
	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:
	
	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.
	
	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.


