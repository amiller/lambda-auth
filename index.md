---
layout: page
title: Authenticated data structures
tagline: Merkleizing all the things
---

{% include JB/setup %}

GPADS is a tool for generating secure authenticated data structure protocols from specifications written in an ordinary programming language. It consists of a modified OCaml compiler and a small library.

Based on a programming language concept, λ● (pronounced "lambda-auth"), presented POPL 2014. 

## What are Authenticated Data Structures (ADSs)?

ADSs are secure protocols for outsourcing data to untrusted parties. Suppose a Client that doesn't have much storage capacity available (maybe it's a mobile device) can communicate to a powerful Server that it doesn't trust. An ADS protocol...

## Example: Binary Search Tree

The following code (from `bintree.ml`) is an example of membership lookup for an authenticated set:


    let rec member x = function
      | Tip -> false
      | Bin a -> let (l,y,r) = (fun x -> unauth x) a in
          if x = y then true else if x < y
          then member x l
          else member x r
    


## Running the examples:

To run the examples...

    $ make driver
    $ ./poorman bintree.ml



