---
layout: page
title: Authenticated data structures
tagline: Merkleizing all the things
---

{% include JB/setup %}

GPADS is a tool for generating secure authenticated data structure protocols from specifications written in an ordinary programming language. It consists of a modified OCaml compiler and a small library.

Based on a programming language concept, λ● (pronounced "lambda-auth"), presented POPL 2014. 

## What are authenticated data structures?

Authenticated data structures are protocols for securely outsourcing data storage to untrusting parties. Typically, the protocol is between a Client and a Server. The Client doesn't want to store very much data (it has limited hard drive capacity), but it also doesn't trust the Server to behave correctly.

## Update Author Attributes

In `_config.yml` remember to specify your own data:
    
    title : My Blog =)
    
    author :
      name : Name Lastname
      email : blah@email.test
      github : username
      twitter : username

The theme should reference these variables whenever needed.
    
## Sample Posts

This blog contains sample posts which help stage pages and blog data.
When you don't need the samples anymore just delete the `_posts/core-samples` folder.

    $ rm -rf _posts/core-samples

Here's a sample "posts list".

<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>

## To-Do

This theme is still unfinished. If you'd like to be added as a contributor, [please fork](http://github.com/plusjade/jekyll-bootstrap)!
We need to clean up the themes, make theme usage guides with theme-specific markup examples.


