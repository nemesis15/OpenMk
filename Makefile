all :
	ghcjs --make src/OpenMk.hs -i src/Menu.hs -o bin/OpenMk
	ghcjs --make src/Categories.hs -i src/Menu.hs -o bin/Categories
	ghcjs --make src/About.hs -i src/Menu.hs -o bin/About
	ghcjs --make src/Help.hs -i src/Menu.hs -o bin/Help
	ghcjs --make src/Politic.hs -i src/Menu.hs -o bin/Politic
	ghcjs --make src/SignUp.hs -i src/Menu.hs -o bin/SignUp
	ghcjs --make src/SignIn.hs -i src/Menu.hs -o bin/SignIn


clean :
	rm -rf src/*.js_hi
	rm -rf src/*.js_o

install :
	cp -R src/php/* bin/php
	cp bin/OpenMk.jsexe/index.html bin/index.html
	cp bin/Categories.jsexe/index.html bin/categories.html
	cp bin/About.jsexe/index.html bin/about.html
	cp bin/Help.jsexe/index.html bin/help.html
	cp bin/Politic.jsexe/index.html bin/politic.html
	cp bin/SignIn.jsexe/index.html bin/signin.html
	cp bin/SignUp.jsexe/index.html bin/signup.html
	cp -R bin/* ~/srv/http/OpenMk
