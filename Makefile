.PHONY: all clean install

JAR := target/organalia-0.1.0.jar
BINDIR := $(or $(BINDIR),$(HOME)/bin)

all: $(JAR)

$(JAR): deps.edn build.clj src/organalia/main.clj
	clojure -T:build uber

clean:
	clojure -T:build clean
	rm -f organalia.jar

install: all
	install -d $(BINDIR)
	install -m 644 $(JAR) $(BINDIR)/organalia.jar
	install -m 755 organalia $(BINDIR)/organalia
