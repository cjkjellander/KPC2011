all: beams

beams:
	erl -make

clean:
	rm -rf ./ebin/*.beam

distclean: clean
	rm *.boot
	rm *.script

bootscripts: beams
	erl -pa ebin/ -noshell -eval 'systools:make_script("reversi-0.1", [local]).' -s init stop
