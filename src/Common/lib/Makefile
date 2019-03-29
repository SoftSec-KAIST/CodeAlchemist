all: libexec.dll

libexec.dll: exec.c
	@gcc -O3 -shared -fPIC $< -o$@

clean:
	rm libexec.dll
