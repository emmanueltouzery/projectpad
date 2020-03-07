# flatpak install org.kde.Sdk/x86_64/5.14
cd "$(git rev-parse --show-toplevel)" || exit 1
# stack ls dependencies | sed "s/ /-/" > flatpak/dependencies-list.txt
# wget -P flatpak https://raw.githubusercontent.com/emmanueltouzery/movie-monad/patch-1/packaging/linux/flatpak/build-module-dependencies.sh
# echo "Please wait, the next action is going to take some time"
# echo "[" > flatpak/dependencies.json
# sh flatpak/build-module-dependencies.sh >> flatpak/dependencies.json
# # remove trailing comma
# sed -i '$ s/},$/\}/' flatpak/dependencies.json # the leading $ makes it apply on the last line only
# echo "]" >> flatpak/dependencies.json
# rm flatpak/build-module-dependencies.sh
flatpak-builder --install repo flatpak/com.github.emmanueltouzery.projectpad.json --force-clean --user
#rm flatpak/dependencies-list.txt
