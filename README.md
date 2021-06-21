# smoke

This is a simple implementation of Jos Stam's
[Stable Fluids](http://pages.cs.wisc.edu/~chaol/data/cs777/stam-stable_fluids.pdf)
paper written in the [Odin](https://odin-lang.org) programming language.

I would like to thank [Morten Vassvik](https://twitter.com/vassvik) for
pointing me to the paper and helping me out with the math and conceptual stuff :)

The code has only been tested on Linux.

# Dependencies

- [odin-gl](https://github.com/vassvik/odin-gl)
- [odin-glfw](https://github.com/vassvik/odin-glfw)

See the repositories for installation instructions. This code
expects the dependencies to be installed in the `shared` package
collection.

- [stb\_truetype](https://github.com/nothings/stb) which is included
directly in this repository and does not need to be installed separately.

# Building

Currently, only Linux has been tested and the code can be built with `make`,
which just builds `stb_truetype` with `clang` and then invokes the Odin
compiler.

# Features to be added

- Boundary conditions
- Better advection algorithm (BFECC, MacCormack, etc)
- Multigrid pressure solver
- At least one-way rigid body coupling
- User interaction
- Use compute shaders
