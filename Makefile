INCLUDE=-pa deps/*/ebin -pa ebin
all:
	rebar get-deps
	rebar compile
run:
	erl $(INCLUDE) -boot start_sasl -name chatik -s chatik_app start -s sync go
deploy:
	erl -heart -detached -noshell -noinput $(INCLUDE) -boot start_sasl -name chatik -s chatik_app start -s sync go
attach:
	erl -name console -hidden -remsh chatik

