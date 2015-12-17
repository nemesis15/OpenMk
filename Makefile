all :
	ghcjs --make src/OpenMk.hs -i src/Menu.hs -o bin/OpenMk
	ghcjs --make src/Categories.hs -i src/Menu.hs -o bin/Categories
	ghcjs --make src/About.hs -i src/Menu.hs -o bin/About
	ghcjs --make src/Help.hs -i src/Menu.hs -o bin/Help
	ghcjs --make src/Politic.hs -i src/Menu.hs -o bin/Politic
	ghcjs --make src/SignUp.hs -i src/Menu.hs -o bin/SignUp
	ghcjs --make src/SignIn.hs -i src/Menu.hs -o bin/SignIn
	cp -R bin/* ~/srv/http/OpenMk
