# The Monad Presentation project

This is a presentation in swedish about monads.  It uses a Yesod
back-end that lets the user evaluate code examples directly in the
browser.

# Installation

Because I'm lazy, I modified some packages directly to make cloudeval
work.  These packages are in deps/ and are ruthlessly hacked.  Some
day I might make actual patches out of the changes I did, but for now,
do this to install cloudeval:

 1. Install vector-0.10.9.2 (from the source in
    vector-0.10.9.1.tar.gz) OR compute the patch between
    vector-0.10.9.1 and vector-0.10.9.2 and apply that patch to the
    latest vector version.  If you don't know how to compile the
    package, extract it and run "cabal install" in the directory.

 2. Install hint-0.3.3.7 (same situation as above)

 3. Install cloudeval itself with a simple "cabal install"

 4. Install MonadRandom-0.1.12.1 (also a hacked package)

The vector and MonadRandom changes do just tag some modules in the
package as trustworthy, so I can use the packages in the
presentation.

The changes to hint are actually necessary for cloudeval to work,
though.

# Running

Simply run "cloudeval 3000" (or "dist/build/cloudeval 3000" if you
didn't install the package) in the source repository.  "3000"
specifies which port to use.  You can access the web page at
http://localhost:3000 (or whatever).
