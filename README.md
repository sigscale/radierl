# Radierl

The radierl project implements a RADIUS protocol in Erlang. It is used
to build embedded RADIUS servers for applications such as authentication,
authorization and accounting (AAA) servers.

# Building the stack

### Clone the repository
	$ git clone https://github.com/sigscale/radierl.git
	$ cd radierl

### Port the radius application to your environment:
	$ aclocal; autoheader; autoconf; automake --add-missing
	$ ./configure

### Build the radius application and documentation:
	$ make

### Run the test suites for the radius application:
	$ make check

# Installing the radius application:

### Install into Erlang system in place
	$ sudo make install

# Building the example servers

### Port the example callbacks application to your environment:
	$ cd examples
	$ aclocal; autoheader; autoconf; automake --add-missing
	$ ./configure

### Build the example callbacks application and documentation:
	$ make

### Run the test suites for the example callbacks application:
	$ make check

# Installing the example callbacks application:
	$ sudo make install

# Running the examples

### Initialize the example callbacks application:
	$ erl
	1> mnesia:create_schema([node()]).
	ok
	2> mnesia:start().
	ok
	3> radius_example:install([node()]).
	
	=INFO REPORT==== 30-Jan-2014::13:49:54 ===
	Created new radius_client table.
	
	=INFO REPORT==== 30-Jan-2014::13:49:55 ===
	Created new radius_user table.
	{ok,[radius_client,radius_user]}

### Start the stack and example applications:
	4> application:start(radius).
	ok
	5> application:start(radius_example).
	ok

Everything here is distributed under Apache License version 2.0 .

