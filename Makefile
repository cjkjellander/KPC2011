all: beams

beams:
	erl -make

clean:
	rm -rf ./apps/reversi/ebin/*.beam

distclean: clean
	rm -f *.boot
	rm -f *.script

bootscripts: beams
	erl -pa ./apps/reversi/ebin/ -noshell -eval 'systools:make_script("reversi-0.1", [local]).' -s init stop
