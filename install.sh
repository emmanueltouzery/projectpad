command -v patch >/dev/null 2>&1 || { echo >&2 "I require the command 'patch' but it's not installed.  Aborting."; exit 1; }

if [ ! -d "persistent-sqlcipher-2.6" ]; then
    echo "setting up persistent sqlcipher integration..."
    wget https://hackage.haskell.org/package/persistent-sqlite-2.6/persistent-sqlite-2.6.tar.gz
    tar xvfz persistent-sqlite-2.6.tar.gz
    rm persistent-sqlite-2.6.tar.gz
    patch -p0 <<'EOF'
--- persistent-sqlite-2.6/persistent-sqlite.cabal	2016-08-14 00:03:34.000000000 +0200
+++ persistent-sqlite-2.6/persistent-sqlcipher.cabal	2017-02-11 10:53:17.086611649 +0100
@@ -1,4 +1,4 @@
-name:            persistent-sqlite
+name:            persistent-sqlcipher
 version:         2.6
 license:         MIT
 license-file:    LICENSE
@@ -39,12 +39,7 @@
     exposed-modules: Database.Sqlite
                      Database.Persist.Sqlite
     ghc-options:     -Wall
-    if flag(systemlib)
-        extra-libraries: sqlite3
-    else
-        c-sources:   cbits/sqlite3.c
-        include-dirs: cbits
-        cc-options:  -fPIC -std=c99
+    extra-libraries: sqlcipher

     c-sources: cbits/config.c

EOF
    mv persistent-sqlite-2.6/ persistent-sqlcipher-2.6/
    mv persistent-sqlcipher-2.6/persistent-sqlite.cabal persistent-sqlcipher-2.6/persistent-sqlcipher.cabal
fi
echo "Will now install the haskell compiler if needed"
stack setup
echo "Will now compile the application"
stack install $*
echo "The application is installed! Run with ~/.local/bin/projectpad"
