Dissentr - A High-Latency Overlay Mix Network for MicroBlogging

Goals:
- Anonymously facilitate microblogging
- Immune to end-to-end attacks

Network:
- Directed, acyclic graph, with out-degree 1
- A 'cascade' is a path containing two terminal nodes
- Cascades build trust, by ensuring that any path will always contain at least 1-trusted node
- Protects against supernode attacks: the NSA can't control the network without compromising trusted nodes

Node:
- Maintains an RSA keypair
- Points to another node