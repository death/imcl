# imcl

[ECL](https://common-lisp.net/project/ecl/main.html) + [IMGUI](https://github.com/ocornut/imgui) = IMCL

First, edit `main.lisp` so that you can load
[Quicklisp](https://www.quicklisp.org/),
[Swank](https://common-lisp.net/project/slime/doc/html/Lisp_002dside.html#Lisp_002dside),
and the IMCL systems.

Then you can build IMCL, set it up to load the `main.lisp` file, and
run the imcl executable:

```sh
$ cmake .
$ make
$ cd bin
$ ln -s ../main.lisp .
$ ln -s ../lisplogo_alien_256.png .
$ ./imcl
```

If you got it working, congratulations! It's still a very immature
project, and there is much to be done. Why not improve it a bit and
submit a pull request?

# License

MIT
