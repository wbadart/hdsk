# Haskell Data Science Kit

![Build status](https://travis-ci.org/wbadart/hdsk.svg?branch=master)

The Haskell Data Science Kit (*HDSK*) project is an attempt to create a
well-documented, well-tested, and performant data science library implemented
in the Haskell language.

Sources suggest that in spite of huge potential for performance gains over
current de facto methods [[1]], adoption of Haskell in the data science
community lags for a variety of reasons, the greatest of which seems to be the
dearth [[2]] of easy-to-use data science libraries (indeed, searching for
"*data science*" on GitHub yields **14** Haskell-language repositories and
**5,807** Python-language repositories [[3]]). This project seeks to mediate
that issue by presenting a *unified* (though modular) library of data science
utilities which support the entire life-cycle of a data science project.

**Disclaimer:** At the time of writing, I am still a beginner in Haskell, and
this project is as much about the above stated goal as it is about me learning
and practicing Haskell itself and the software development ecosystem around it.
So, I make no guarantees that I will give the most optimal or idiomatic
solution to any given function (and in cases when I don't, pull requests are
gladly welcomed!).

[1]: https://izbicki.me/blog/hlearn-cross-validates-400x-faster-than-weka
[2]: https://www.linkedin.com/pulse/haskell-data-science-good-bad-ugly-tom-hutchins
[3]: https://github.com/search?q=data+science+language%3AHaskell&type=Repositories


## Roadmap

I've sketched out a four-step development plan which I believe will maximize
the potential for adoption of this library. Careful attention has been paid to
maximizing modularity, and therefore generality. Ultimately, I hope this
solution could be dropped into any existing project in any language in a
near-seamless manner.

### 1. Develop the core library

This first step will build out the core functionality of the library in a pure
Haskell context. The key deliverable will be a packaged Haskell library
containing (at least a minimal demonstrative set of) the target functions of
the library. Secondary deliverables will include comprehensive tests and
documentation, and benchmarks. My main learning focus will be on the Haskell
language itself and on its software packaging and distribution ecosystem.

This implies an initial research phase which will begin to identify the
functions the library will implement. I'll be looking largely to NumPy [[4]] and
Scikit-learn [[5]] for inspiration and outlines here.

[4]: https://numpy.org
[5]: https://scikit-learn.org

### 2. Create a foreign function interface (exporting)

Part of the value proposition of this project (which I hope will improve
adoption) will be compatibility with non-Haskell projects. Other than the
(un)availability of libraries, the learning curve of the language itself is a
hindrance to the adoption of Haskell in the data science community [[2]].
By exposing this library to other projects in other languages, we can remove
the language learning curve from the equation and offer a more gentle
introduction to the applications of Haskell within data science.

The ultimate goal (i.e. primary deliverable) would be a library of bindings in
a target language that allow for core library functions to be called as native
functions while executing in the Haskell runtime. I don't know just yet how
I'll go about this, but two options I've started investigating are outlined
below:

1. A combination of Haskell's native Foreign Function Interface [[6]] with
   *nh2*'s `call-haskell-from-anything` tool [[7]]. Once the library in the
   target language is configured and/ or generated, its functions appear to
   behave normally. However, this approach would require (in addition to
   regular maintenance of the core library) maintenance of the FFI (which may
   live in a separate module which simply re-exports core functions) and
   maintenance of of serialization handling in the target language.

2. Wrapping the library in an Apache Thrift [[8]] service. This option is
   attractive because of the robust existing ecosystem of projects (and online
   documentation) under this framework. I also have a little personal
   experience with it. The problem I foresee from my initial research is that
   using Thrift will impose a heavy and opinionated structure on the project
   and make it more difficult to generate the client-language libraries.

   On the other hand, this RPC/ service-oriented approach may lend itself
   better to a more diverse set of clients (e.g. being able to use HDSK from
   within browser-side JavaScript [[9]], though see next section for more).

To evaluate these (and any other potential approaches), I will consider 1) the
performance (particularly in the serialization options) and 2) the ease-of-use
from the point of view of the target-language client.

At this point, it will be possible to begin testing for one of the secondary
goals of the project: to provide more performant data science utilities than
the current de facto solutions in the target languages.

In this stage, I hope to learn about how Haskell interoperates with other languages and to develop and understanding of remote procedure call frameworks and where their most appropriate applications are.

[6]: https://wiki.haskell.org/Foreign_Function_Interface
[7]: https://github.com/nh2/call-haskell-from-anything
[8]: https://thrift.apache.org
[9]: https://thrift.apache.org/lib/js

### 3. Draft a JSON-formatted Web API

That is, set the library down behind a web server and wire up each function to
an endpoint. By my current conception, every top-level function in the library
will have the same domain and range: data values [[10]]. Specifically, data
serializable to the JSON format (numbers and lists of numbers primarily).

This interface translates quite seamlessly to the Web, and by providing the
library as a service, we provide an alternative to the above architecture which
makes no assumptions about or requests of the client. It simply provides the
operations as a web service (perhaps you could see it as a Web-generalized
version of the above Thrift proposal).

[10]: https://youtu.be/-6BsiVyC1kM?t=8m6s

### 4. Containerize the components of the project

I suggest this 1) because improving portability may improve adoption, and 2)
because I'm interested in learning how to do this.

By publishing the library in a container, we open the door for developers to
(for example) deploy the generated Thrift server with a single `docker` command
for use in their projects. We really want to make the barrier to entry as low
as possible, and I see this as a way of doing that.


## License

You'll notice a key theme in this document has been **promoting adoption.** As
such, I'm developing and eventually releasing this project under the
BSD-3-Clause [[11]] license, due to its general permissiveness. This is also
one of the more popular licenses among the Haskell community [[12]].

Please see [LICENSE](./LICENSE) for the full text.

[11]: https://opensource.org/licenses/BSD-3-Clause
[12]: https://wiki.haskell.org/How_to_write_a_Haskell_program
