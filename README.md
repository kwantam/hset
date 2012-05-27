# hset

The [Game of Set](http://www.setgame.com/set/) implemented in Haskell with a CGI interface.

There are a bunch of different implementations here that evolved into the CGI binary. I've left them here for posterity.

## Building

To use the CGI script, you must first create the card deck. As long as you've got ImageMagick installed, you can just `make cards` and it'll generate the deck; you can change the destination directory by editing the `IMGDIR` variable in the Makefile.

While you're in the Makefile, set the `IMGURL` variable to the base URL from which the card deck can be accessed. Once that's done, you can `make` and you should be set. Put make the images accessible at `IMGURL`, and install `build/HSetHTML.cgi` in your cgi-bin directory, and you should be off and running.
