A JVM for the web. Bring back the applet! /s

**This project is not, in any way, affiliated with or endorsed by Oracle, Inc. or the OpenJDK project.**

## Why?

I (Timothy) have three overarching goals for the project, in roughly descending order: 1. to learn about compilers and runtimes; 2. to make it easy to run and interface with modern, OpenJDK-based Java projects in the browser, without extensive rewrites; 3. to push the envelope of WebAssembly a bit.

Regarding (2), existing solutions like [TeaVM](https://www.teavm.org/) and [GWT](https://www.gwtproject.org/)/J2CL are fantastic, but they require porting code, and make (reasonable) correctness compromises for the sake of performance (*e.g.*, omitting array bounds checks). In particular, they are compiled ahead of time, which rules out libraries that generate Java bytecode at runtime. (This includes modern OpenJDK.)

[CheerpJ](https://labs.leaningtech.com/blog/cheerpj-3.0) is closer to what I desire, and is an impressive piece of tech, but has some key deficiencies: it JITs to JavaScript, which is not sandboxable; it only supports Java 8 (although future Java version support is planned); its implementation of Java is incomplete (no ArrayStoreException, incomplete error messages, no line numbers in backtraces, *etc.*); and above all, it is closed source, requiring a license to use commercially. It's also fairly slow and heavyweight: bjvm starts up in a few hundred milliseconds, while theirs takes seconds.

Message me on Discord if you are interested in collaborating: forevermilk#0001. Java (testing), TypeScript (JS interop), C (VM and natives), C++ (testing) are the main languages used.

### Achieved
- JDK 23 support
  - VarHandle, MethodHandle, invokedynamic, etc. – all bytecodes are implemented (besides deprecated jsr/ret)
- Correct behavior in erroneous cases (including array out of bounds, stack overflow, memory exhaustion, array store exception, *etc.*)
- Line numbers in backtraces
- Some classfile verification (incomplete)
- Single-threaded compacting GC
- Integration with JS GC through handles and FinalizationRegistry
- Fast [tail call–based](https://blog.reverberate.org/2021/04/21/musttail-efficient-interpreters.html) interpreter (0–30% slower than interpreter-only HotSpot on native, ~2–3x slower on WASM)
- Multithreading support via context switching on a single thread (including special methods like Object.wait, Object.notify, Unsafe.park, *etc.*)
- Preëmption of threads
- Easy-to-use JavaScript interface
- Scheduler with JS event loop integration
- Basic features of class loaders (e.g. URLClassLoader works)
- DTrace probes for performance analysis
- Stack-sampling profiler

### Goals

- TypeScript type generation (including generics, which will require parsing the Java source)
- JIT compilation to WebAssembly with SSA-based IR
- JIT compilation to x86 and ARM64 (lower priority – more for fun)
- Generational GC (probably not concurrent though – that's too hard)
- Educational features
  - Emulated networking support
  - Debugger support
  - ?? (suggestions welcome)
- Easy integration with JS promises and callbacks
- True multithreading via Web Workers/SharedArrayBuffer
- Full implementation of the JVM spec (including esoteric things like classfile verification)
- Some JNI compatibility (e.g. for LWJGL. Modern Minecraft in the browser would be cool.)

### Non-goals (at least for now)

- Robust and/or secure desktop JVM – but useful for testing/debugging
- Swing, AWT, *etc.* – but not ruling it out
- "Elegant" code (roughly: correctness > performance > purity). We're using C instead of Rust for a reason.

## Notes to self

### JITing to WebAssembly

- General method signature: `return_t (*compiled)(bjvm_thread *thread, bjvm_cp_method *method, ...args)`
- Generated on the fly for each argument types/return type combination
- Trampolines between interpreter and JIT code (either pre-generated or generated at runtime)
- Method is responsible for setting up its own stack frame and in the case of de-opt or interruption, generating all interpreter stack frames
- Methods which are not compiled will have a generated implementation that delegates appropriately to the interpreter


### Useful

```
clang-format -i test/**/*.cc test/**/*.h vm/**/*.c vm/**/*.h natives/**/*.h natives/**/*.c
```

### Overloaded Java functions

When overloaded functions are compiled into JS, overloads with the same number of arguments will be disambiguated with a generated suffix. For example, if there is a function `f(double, double)` and `f(double, Object)`, then there will be generated functions `f_$D$D` and `f_$D$Ljava_lang_Object`. To be precise, all arguments are prefixed with a `$`, and class/interface names are prefixed with `$L`, while the package separator is replaced with an underscore. This can still generate some collisions; in this case, the function called is unspecified.

Disambiguated constructors are called "<init>_$...". Yes, the syntax is gross.

A function named `drop` is called `drop$` to avoid a collision with the `drop` function, which takes a handle and drops it from use (instead of automatic management).

Variadic functions are not yet supported; the variadic argument is 
