cd "$(git rev-parse --show-toplevel)" || exit 1
stack ls dependencies | sed "s/ /-/" > flatpak/dependencies-list.txt
wget -P flatpak https://raw.githubusercontent.com/lettier/movie-monad/d1f58779644c13528428fa9c294585d6b897d847/packaging/linux/flatpak/build-module-dependencies.sh
sh flatpak/build-module-dependencies.sh > dependencies.json
rm flatpak/build-module-dependencies.sh
rm flatpak/dependencies-list.txt
