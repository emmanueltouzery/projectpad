early beginning of a project.

the icons come from http://www.glyphicons.com (will be credited in the app when the app is far enough)

The storage is done through SQLcipher to have a safely encrypted data storage.
That does complicate the installation of the application though.

You must first install sqlcipher itself. If your distribution doesn't have packages, you'll have to compile it:
https://github.com/sqlcipher/sqlcipher#compiling
(dynamic linking worked best for me)

Then you'll need the persistent-sqlcipher haskell package. That package however doesn't really exist. You'll have to fetch the
persistent-sqlite package source, and modify the cabal file. Simply open the persistent-sqlite.cabal and change the name from
persistent-sqlite to persistent-sqlcipher, and the extra-libraries section:

    -    if flag(systemlib)
    -        extra-libraries: sqlite3
    -    else
    -        c-sources:   cbits/sqlite3.c
    -        cc-options:  -fPIC
    +    extra-libraries: sqlcipher

To install persistent-sqlcipher only to the projectpad sandbox you can install it from the persistent checkout folder running:

    cabal --sandbox-config-file=<projectpad dir>/cabal.sandbox.config install

You can also compile and run against a sqlcipher that was not installed globably on the computer using LD_LIBRARY_PATH:

    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:<sqlcipher install dir>/lib/ cabal build

The same to run the application.
