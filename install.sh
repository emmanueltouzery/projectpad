command -v patch >/dev/null 2>&1 || { echo >&2 "I require the command 'patch' but it's not installed.  Aborting."; exit 1; }

if [ ! -d "persistent-sqlcipher-2.6.4" ]; then
    echo "setting up persistent sqlcipher integration..."
    wget https://hackage.haskell.org/package/persistent-sqlite-2.6.4/persistent-sqlite-2.6.4.tar.gz
    tar xvfz persistent-sqlite-2.6.4.tar.gz
    rm persistent-sqlite-2.6.4.tar.gz
    patch -p0 <<'EOF'
--- persistent-sqlite-2.6.4/persistent-sqlite.cabal	2017-12-02 22:27:44.000000000 +0100
+++ persistent-sqlite-2.6.4/persistent-sqlcipher.cabal	2018-01-04 18:15:27.309943866 +0100
@@ -1,4 +1,4 @@
-name:            persistent-sqlite
+name:            persistent-sqlcipher
 version:         2.6.4
 license:         MIT
 license-file:    LICENSE
@@ -44,15 +44,7 @@
     exposed-modules: Database.Sqlite
                      Database.Persist.Sqlite
     ghc-options:     -Wall
-    if flag(systemlib)
-        if flag(use-pkgconfig)
-            pkgconfig-depends: sqlite3
-        else
-            extra-libraries: sqlite3
-    else
-        c-sources:   cbits/sqlite3.c
-        include-dirs: cbits
-        cc-options:  -fPIC -std=c99
+    extra-libraries: sqlcipher
 
     c-sources: cbits/config.c

EOF
    mv persistent-sqlite-2.6.4/ persistent-sqlcipher-2.6.4/
    mv persistent-sqlcipher-2.6.4/persistent-sqlite.cabal persistent-sqlcipher-2.6.4/persistent-sqlcipher.cabal
fi
echo "Will now install the haskell compiler if needed"
stack setup
echo "Will now compile the application"
stack install $*
xdg-icon-resource install --size 64 projectpad-64.png projectpad --novendor
xdg-icon-resource install --size 96 projectpad-96.png projectpad --novendor
xdg-icon-resource install --size 128 projectpad-128.png projectpad --novendor
xdg-desktop-menu install com.github.emmanueltouzery.projectpad.desktop --novendor
echo "The application is installed! Run with ~/.local/bin/projectpad"
