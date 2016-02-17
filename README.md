# ProjectPad

![Main view screenshot](https://raw.githubusercontent.com/wiki/emmanueltouzery/projectpad/main_pic.png)

## Description

ProjectPad allows to manage secret credentials and server information that you need to handle as a software developer. List of servers, list of point of interests on those servers (applications, log files, databases, servers). It will securely store paswords and keys.
It will also run commands (locally or on SSH servers), open terminals on remote SSH servers and open windows remote desktop sessions in one click.

## Security

The data is securely stored on-disk using [SQLcipher][], which uses 256-bit AES. You must enter a password to encrypt and unlock the database everytime ProjectPad starts. However no particular care is taken to protect the passwords in-memory: if someone can
dump the memory of ProjectPad, they will be able to extract passwords from it.
Your database password is never stored to disk. When you open SSH shells, the password to the remote server is briefly stored to disk in a file with 700 permissions, which will echo the password when executed (the 700 permissions means it's readable and executable by your user only). When you run SSH commands in a local terminal (tail, less, vim), a file with 700 permissions is briefly stored to disk (in 1.0 that file is never deleted, just overwritten everytime), which contains the remote username, remote host and the command to run. When you open a Windows remote desktop session, the password is piped in cleartext to the rdesktop process.

## Credits
the icons come from http://www.glyphicons.com

The storage is done through SQLcipher to have a safely encrypted data storage.
That does complicate the installation of the application though.

## Extra screenshots

![Edit server](https://raw.githubusercontent.com/wiki/emmanueltouzery/projectpad/edit_server.png)
![Action ring](https://raw.githubusercontent.com/wiki/emmanueltouzery/projectpad/action_ring.png)
![Edit project](https://raw.githubusercontent.com/wiki/emmanueltouzery/projectpad/edit_project.png)
![Search](https://raw.githubusercontent.com/wiki/emmanueltouzery/projectpad/search.png)

## Installation

On Fedora, you'll need to install the `qt5-qtdeclarative-devel`, `qt5-qtquickcontrols` and `qt5-qtgraphicaleffects` packages.

You should also install some unicode font because some unicode symbol glyphs are used when editing notes. On Fedora you can install `gdouros-symbola-fonts`.

You must first install sqlcipher itself. If your distribution doesn't have packages, you'll have to compile it:
https://github.com/sqlcipher/sqlcipher#compiling
(dynamic linking worked best for me)
If you have packages, you'll need the -dev or -devel version.

Then you need to [install stack][], and to build you can use simply:

    stack install

Add `~/.local/bin` to your `PATH` then you can run the app simply with the `projectpad` command.

[install stack]: http://docs.haskellstack.org/en/stable/README.html#how-to-install
[SQLcipher]: https://www.zetetic.net/sqlcipher/
