OUTDIR = $(shell pwd)/bin

all: CodeAlchemist

CodeAlchemist:
	dotnet build -c Release -o $(OUTDIR)

clean:
	dotnet clean
	rm -rf $(OUTDIR)
