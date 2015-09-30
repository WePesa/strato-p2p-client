
#ethereumH

[![BlockApps logo](http://blockapps.net/img/logo_cropped.png)](http://blockapps.net)

The blockchain is the underlying technology behind Bitcoin....  But, blockchain technology goes way beyond just being an "online currency".  The 
blockchain is a distributed system which can be used to verify and guarantee stated claims (account balance, resource ownership, legal contracts, 
etc).  The Ethereum blockchain allows Turing Complete code snippets to run on a blockchain, giving the user the fullest leeway to experiment 
with all imagined functionality.  (see http://ethereum.org for more info).

This package provides a tool written in Haskell to allow you to connect to the Ethereum blockchain (see http://gavwood.com/paper.pdf for detail 
of how this works).  You can install the tool as follows

    > cabal install ethereum-client-haskell

and run it as follows

    > ethereumH

This is a first release, and should still be considered as a sort of alpha (albeit, almost fully working), in part because the Ethereum spec 
itself is still a moving target.  That being said, once you run the program, the client will connect to the current testnet and start receiving
and processing blocks (which you will soon see as a series of fast moving messages up the screen).

If you look at the messages, you will see that....

- The client is downloading all the blocks from the testnet.
- Transactions are being processed, and balances are being updated.
- Transactions with code are being run in the Ethereum virtual machine (the code for the VM is in Blockchain.VM.*).
- Once the client has loaded all the blocks, it will start to mine.

If you fiddle with the source code, you can additionally....

- Submit basic transactions.
- Write Ethereum code snippets (which you can write using a specially crafted DSL, which can be compiled to VM code).
- Experiment with which peer-to-peer network to connect to.

I've included a sample private key to get things started (which is, of course, not going to ever be used by me in the real world :) ).  You might 
want to change that before you do any serious work.

----------

Things are moving fast in the Ethereum world, and the testnet is an evolving beast....  I haven't written any code to seriously deal with peer
discovery yet, and have just put in a bunch of hardcoded ip addresses (with one that I know is working today, anointed as the default).  If
you find that you can't connect to the testnet, you might want to play with the hardcoded addresses in the Blockchain.PeerUrls module.  You can
find current peer ip addresses at

http://ethergit.com (click the "network peers" link).

You can choose peers other than the default by supplying a parameter to the ethereumH program.

    > ethereum 4

(where the param is an index in the hardcoded list....  to select the entry).

----------

Known Problems:

- The program is still single threaded, so mining and block synchronization all happen sequentially....  This means that once mining starts, it 
will block until done (even after another peer may have succeeded).  To give the client a chance to load all the new data, mining only is initiated
every other block right now (ie- block 1, mine, block2 synch, block3 mine, etc).

- The VM code needs a bit more testing....  It works pretty well, but every once and a while, a split in the chain (relative to the other 
clients) occurs.  To detect these problems, I deliberately end the program when this happens....  This sort of looks like a crash, but the ides is
that by forcing me to deal with these issues immediately, all such problems will be purged.

- I don't support the Whisper protocol at all yet.

- I can't mine a block with the new logging mechanism (although I can synch against other peer mined blocks, so this isn't as bad as it sounds
right now).

- You still need to fiddle with source code to submit a transaction (although this might be considered a feature....  since the Ethereum code
is compiled from a Haskell DSL language, and is fully type checked at compile time).

- As mentioned above, the Ethereum network is changing very fast, so what works today might not work tomorrow....





