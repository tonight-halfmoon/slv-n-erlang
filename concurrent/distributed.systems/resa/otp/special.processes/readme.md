## How to play with RSSP

### Make
0 - Compose an Emakefile and save it in root level
0 - Compose a .app file and save it in ebin/ dir
1 - erl -make at root level
2 - erl -pa ebin/ at root level
3 - application:load(rssp).
4 - application:start(rssp).

### Generate Boot Script
0 - Compose a .rel file and save it in ebin/ dir
1 - erl -make at root level
2 - systools:make_script("rssp", [local]). at ebin/ dir
3 - erl -boot rssp-1 