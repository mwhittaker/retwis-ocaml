default: all

all:
	corebuild -pkgs async server.byte client.byte

clean:
	rm -f *.byte
	rm -rf _build
