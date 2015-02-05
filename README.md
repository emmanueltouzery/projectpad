# ProjectPad

* [Main view screenshot][]

## Description

ProjectPad allows to manage secret credentials and server information that you need to handle as a software developer. List of
servers, list of point of interests on those servers (applications, log files, databases, servers). It will securely store
paswords.
It will also run commands (locally or on SSH servers), open terminals on remote SSH servers and open windows remote desktop
sessions in one click.

## Security

The data is securely stored on-disk using [SQLcipher][], which uses 256-bit AES. You must enter a password to encrypt and unlock
the database everytime ProjectPad starts. However no particular care is taken to protect the passwords in-memory: if someone can
dump the memory of ProjectPad, they will be able to extract passwords from it.
Your database password is never stored to disk. When you open SSH shells, the password to the remote server is briefly stored
to disk in a file with 700 permissions, which will echo the password when executed (the 700 permissions means it's readable and executable by your user only). When you open a Windows remote desktop session, the password is piped in cleartext to the rdesktop process.

## Credits
the icons come from http://www.glyphicons.com (will be credited in the app when the app is far enough)

The storage is done through SQLcipher to have a safely encrypted data storage.
That does complicate the installation of the application though.

## Extra screenshots

[Edit server][]
[Action ring][]

## Installation
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

[Main view screenshot]: https://raw.githubusercontent.com/wiki/emmanueltouzery/projectpad/main_pic.png
[Edit server]: https://raw.githubusercontent.com/wiki/emmanueltouzery/projectpad/edit_server.png
[Action ring]: https://raw.githubusercontent.com/wiki/emmanueltouzery/projectpad/action_ring.png
[SQLcipher]: https://www.zetetic.net/sqlcipher/
