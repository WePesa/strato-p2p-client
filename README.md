
#ethereumH

[![BlockApps logo](http://blockapps.net/img/logo_cropped.png)](http://blockapps.net)

The blockchain is the underlying technology behind Bitcoin....  But, blockchain technology goes way beyond just being an "online currency".  The 
blockchain is a distributed system which can be used to verify and guarantee stated claims (account balance, resource ownership, legal contracts, 
etc).  The Ethereum blockchain allows Turing Complete code snippets to run on a blockchain, giving the user the fullest leeway to experiment 
with all imagined functionality.  (see http://ethereum.org for more info).

This package provides a tool written in Haskell to allow you to connect to the Ethereum blockchain (see http://gavwood.com/paper.pdf for detail 
of how this works).

----------

Things are moving fast in the Ethereum world, and the testnet is an evolving beast....  I haven't written any code to seriously deal with peer
discovery yet, and have just put in a bunch of hardcoded ip addresses (with one that I know is working today, anointed as the default).  If
you find that you can't connect to the testnet, you might want to play with the hardcoded addresses in the Blockchain.PeerUrls module.  You can
find current peer ip addresses at

http://ethergit.com (click the "network peers" link).

You can choose peers other than the default by supplying a parameter to the ethereumH program.

    > ethereum 4

(where the param is an index in the hardcoded list....  to select the entry).
