Install dependencies (ArchLinux):
```
apt install libseccomp-dev
```

Install dependencies (Ubuntu):
```
apt install libseccomp-dev
```

Compile:
```
gcc seccomp-gen.c -lseccomp -Wall -pedantic -o seccomp-gen
```

Run:
```
./seccomp-gen
```

At last step, a file named `seccomp.bpf` will be generated.
This seccomp filter can then be used in other apps (e.g. bubblewrap).

