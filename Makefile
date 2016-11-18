all:
	haxe build.hxml
	neko main.n
	pushd demo ; haxe web.hxml ; popd
