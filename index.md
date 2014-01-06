---
layout: page
title: Authenticated data structures
tagline: Merkleizing all the things
---

{% include JB/setup %}

λ● (pronounced "lambda-auth") is a tool for generating secure "Authenticated Data Structure" protocols from simple specifications written in an ordinary programming language (OCaml).

The tool consists of a patched OCaml compiler, and is based on a programming language design presented at POPL 2014.

## What's an Authenticated Data Structure?

Authenticated Data Structures (ADSs) are protocols for outsourcing data to untrusted parties. For example, suppose a Client doesn't have much storage capacity (maybe it's a mobile device) but can communicate with a powerful Server it doesn't trust. With an ADS, the Client only has to store a tiny amount of data, yet is guaranteed that its queries are answered correctly.

<div>
<img style="width:500px;display:block;margin-left:auto;margin-right:auto" src="{{ BASE_PATH }}/assets/graphic.png" />
</div>

ADSs work by augmenting ordinary data structures with collision-resistant cryptographic hashes. 

## Example: Binary Search Tree

Creating an ADS protocol in λ● is as easy as writing an ordinary data structure program in OCaml. The following code (from `bintree.ml`) is an example of membership lookup in an authenticated set of integers:

<?prettify lang=ml?>
    let rec (member : 'auth tree -> int -> bool) x = function
      | Tip -> false
      | Bin a -> let (l,y,r) = (fun x -> unauth x) a in
          if x = y then true else if x < y
          then member x l
          else member x r
<br>

The only non-standard syntax is the `'auth` type operator and `unauth` keywords, which are used to provide hints on where to compress the data structure by applying hashes. The λ● compiler automatically generates executables for both the Client and the Server from this single piece of input code.

## Running the examples

Get the most recent version of λ● from github:

    $ git clone https://github.com/amiller/ads-pl

To run the examples, you will first need a copy of ocaml (trunk) installed, and the source directory should be located in the previous directory as `../ocaml-trunk`.

Next, run the following commands:

    $ make                   # Builds the compiler
    $ make prover            # Builds the prover examples
    $ make verifier          # Builds the verifier examples

<font size="-1"><em><b>WARNING Although the topic here is secure software, this is a research prototype, it has not been audited, and you should not use it in production for anything serious.</b></em></font>


