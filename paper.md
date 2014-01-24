---
layout: page
title: "Publications"
description: "asdf"
---
{% include JB/setup %}


<div>
<font size="+1"><b>Authenticated Data Structures, Generically</b>.
  Andrew Miller, Michael Hicks, Jonathan Katz, and Elaine Shi.
  In <i>Proceedings of the ACM Conference on Principles of
  Programming Languages (POPL)</i>, January 2014.
  To appear.</font>
<p>An authenticated data structure (ADS) is a data structure whose
  operations can be carried out by an untrusted <em>prover</em>, the
  results of which a <em>verifier</em> can efficiently check as
  authentic. This is done by having the prover produce a
  compact proof that the verifier can check along with each query result.
  ADSs thus support outsourcing data maintenance and processing tasks
  to untrusted servers without loss of integrity. Past work on ADSs
  has focused on particular data structures (or limited
  classes of data structures), one at a time, often with support only
  for particular operations. This
  paper presents a generic method, using a simple extension
  to a ML-like functional programming language we call lambdaAuth
  with 
  which one can program authenticated operations over any data
  structure constructed from standard type constructors, including
  recursive types, sums, and products. The programmer writes the data
  structure largely as usual; it can then be compiled to code
  to be run by the prover and verifier. Using a formalization of
  lambdaAuth
  we prove that all well-typed lambdaAuth programs result in code that is
  secure under the standard cryptographic assumption of
  collision-resistant hash functions. We have implemented
  our approach as an extension to the OCaml compiler, and have used it
  to produce authenticated versions of many interesting data
  structures including binary search trees, red-black trees, skip
  lists, and more. Performance experiments show that our approach is
  efficient, giving up little compared to the hand-optimized data
  structures developed previously.
  </p><p>
  [ <a href="http://www.cs.umd.edu/~amiller/gpads/gpads.pdf">.pdf</a> ] 
  [ <a href="http://www.cs.umd.edu/~amiller/gpads/gpads-full.pdf">full version (pdf)</a> ]
  [ <a href="https://docs.google.com/presentation/d/1Ycqpsm6an-jvQk3SEHB130JlyGiWJEDZ7Zj-eVMe3ss/edit?usp=sharing">slides</a> ]
  [ <a href="https://github.com/amiller/ads-pl">code</a> ]
<a name="miller14gpads"></a></p>
<font size="-1">
<pre>
@INPROCEEDINGS{miller14gpads,
  AUTHOR = {Andrew Miller and Michael Hicks and Jonathan Katz and Elaine Shi},
  TITLE = {Authenticated Data Structures, Generically},
  BOOKTITLE = {Proceedings of the {ACM} Conference on 
    Principles of Programming Languages (POPL)},
  MONTH = JAN,
  YEAR = 2014,
  NOTE = {To appear}
}
</pre>
</font>
</div>