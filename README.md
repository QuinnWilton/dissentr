Note: This project was created as part of a 48-hour hackathon - and primarily as a proof of concept. While the ideas may be sound, and the prototype may work as designed, the protocols involved in this specific project have not been peer-reviewed, and so I cannot recommend that the network be used for anything requiring serious privacy.

# Dissentr
## A High-Latency Overlay Mix Network

Essentially, Dissentr is a security-minded network, inspired by Tor, with a few important characteristics which serve to differentiate it.

### High-Latency

Tor is a low-latency network. This makes it ideal for real time activities like web browsing, but as a result, opens it up to attacks involving large-scale traffic analysis methods known as end-to-end correlation. In these attacks, an adversary with the ability to analyze massive amounts of traffic in a short period of time is able to match up traffic entering the network with the corresponding traffic which will inevitably soon exit it.

Dissentr manages to protect against these sorts of attacks by being engineered as a high-latency network. Assuming any given node has not been compromised, that node will intentionally hold off on forwarding its traffic to the next node in the network until it is able to forward a large amount of data in bulk, rendering the aforementioned end-to-end correlation far less feasible. For an excellent discussion on this attack, and possible countermeasures, see [Practical Traffic Analysis:
Extending and Resisting Statistical Disclosure](http://freehaven.net/doc/e2e-traffic/e2e-traffic.pdf).

### Cascades
Much like any mix network, Dissentr models its network as a graph of nodes, each responsible for handling the relay of traffic as it moves along some path through the network. Where Dissentr differs from a network such as Tor is in how this path is constructed. In Dissentr, the network is constructed out of cascades (A term I first heard described by Ian Goldberg, but I've been unable to pin down an original source for): essentially directed, acyclic sub-graphs, in which a node defines a set of "trusted" nodes, through which they are willing to relay traffic through. Dissentr simplifies this model by only allowing for nodes of degree 1, at this time. This construction brings about a number of useful results:

1. In the event that a node is known to be compromised, individual nodes are allowed the ability to either remove themselves from a cascade, or bypass untrusted nodes entirely, without the necessity of a trusted third-party.
2. The network is protected from "supernode invasions," in which an attacker floods the network with compromised nodes, in the hopes of either endangering the network's health, or placing the security of users passing through their nodes at risk of traffic interception, and subsequent analysis. This can be guaranteed because cascades are constructed by virtue of a measure of trust between node-operators, and so long as there exists some non-zero subset of trusted operators, they retain the ability to form a cascade of their own, effectively shutting out the efforts of such an attacker.

### Use-Cases
As mentioned previously, the high-latency nature of the network causes a shift in the sorts of activities best facilitated by its use, however, there do exist some unique opportunities which I have neither seen implemented in the context of a mix network, nor discussed in the literature.

A personal favourite idea revolves around creating a platform for political blogging, which, assuming a noisy enough network, would offer political dissidents the ability to freely write about issues of corruption or government abuse, without many of the risks associated with using a lower-latency network like Tor. If it takes a week for a blog post to appear in circulation after the author posts it to the network, it becomes magnitudes more difficult for any assailant to trace the authorship of that blog post - especially if that author never visited the website which hosts their content in the first place!

It also becomes a fairly trivial exercise to adapt the network to act as a mixing service for digital currency such as Bitcoin. Furthermore, by breaking the network into a number of smaller, disjoint networks for that purpose, one is be able to counter many of the current attacks which target existing mixing services.

### Cryptosystem
I again emphasize that the cryptosystem in place is the result of a rather rushed 48-hour hackathon - in a production system, I would recommend implementing a peer-reviewed cryptosystem, such as the very lightweight [Sphinx](http://www.cypherpunks.ca/~iang/pubs/Sphinx_Oakland09.pdf), or, pending their coming proof of security, the recently proposed [Ibis](https://ibis.uwaterloo.ca/). That being said, Dissentr works as follows:

1. Every node in the network maintains an RSA-keypair, with the public key being exposed to every node in a given cascade.
2. When a client wishes to send a message M through the network, they choose some cascade C.
3. For each node in the cascade, beginning with the exit node, and continuing through to the entrance node, the client generates an AES CFB128 key, which it uses to encrypt M. The key is then encrypted using that node's public RSA key.
4. M, now encrypted with AES CFB128 for every node in the cascade, is then passed to the entrance node along with the encrypted AES keys. The entrance node then uses its private RSA key to decrypt the AES key, so that it can subsequently decrypt M, yielding yet another cipher text.
5. This process is repeated for every node in the cascade, until the final node decrypts M to a plaintext, which it then handles accordingly.

### Building and Running it

If, after all of my warnings, you still want to see it in action, it's dead-easy to get setup. All you'll need is Erlang installed (Tested on R16B02), along with [Elixir](http://elixir-lang.org/). From there, you'll want to invoke the following from within Dissentr's directory, on every machine you want to host a node:

    iex --sname {Any name, different per machine} --cookie {Any string, common between all machines} -S mix
    
This will stick you into a REPL, loaded with Dissentr's namespaces and dependencies. Sorry, there's no interface yet. From there, if you're using more than one machine, you'll want to link them all together, by running the following on every machine you want to host a node on. Since Erlang node connections are transitive, you won't have to do this for every pair of nodes.:

    :net_adm.ping(binary_to_atom(hostname)
    
The hostname in question can be found in the iex prompt. Most likely it will be something@domain.

Now, just spawn a few nodes to create a network. I've got some temporary methods in place for making this easy, using some hardcoded keys stored in example_data/ for testing. Ideally, each node will be hosted on a different machine, but for testing purposes it doesn't matter. Within your prompt, execute the following:

    Dissentr.Cascade.add_node(:node1, nil, 1)
    Dissentr.Cascade.add_node(:node2, :node1, 2)
    Dissentr.Cascade.add_node(:node3, :node2, 3)
    Dissentr.Cascade.add_node(:node4, :node3, 4)
    Dissentr.Cascade.add_node(:node5, :node4, 5)
    
Finally, to send an encrypted message, run the following, substituting the node and message as desired:

    Dissentr.Cascade.mix(:node3, "Something, something, NSA")
    
If all went well, you should see a debug statement print out the plaintext method, on the machine which is hosting :node1
