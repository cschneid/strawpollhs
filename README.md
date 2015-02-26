A simple toy project to reimplement strawpoll.me

This software is not, and probably will never be usable by anybody except as an example

## My Goals:

* Use GHCJS and Blaze-React to make a responsive and easy to develop frontend
* Minimize boilerplate by sharing data types between front and backend
* Explore using AJAX and also WebSockets
* Learn more about production application database management in Haskell.  What does deployment look like? How to manage change?
* See how much code can be shared between client and server

## Compiling:

Build the client: 

    cabal install -j1 -fclient --ghcjs

Build the server:

    cabal install -j1 -fserver

Link static files:

    cd static
    ln -s ../dist/..../client.jsexe/* .

Run Server:

    dist/.../server
    # Visit port 7000
