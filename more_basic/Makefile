OPTIONS = -j 4 -use-menhir -use-ocamlfind -yaccflag --table -pkgs menhirLib,str,unix -no-links
FILESRC = Main.native
EXEC = main

all: $(EXEC)

.PHONY: clean $(EXEC) redo

redo: clean autogenerate $(EXEC)

clean:
	@-_build/sanitize.sh
	ocamlbuild -clean
	@-rm -f src/Parser.automaton
	@-rm -f src/Parser.conflicts
	@-rm -f src/Parser.dot

autogenerate:
	menhir src/Parser.mly --list-errors > src/ParserMessages.messages

automaton:
	menhir src/Parser.mly --dump --explain # --graph
	vim src/Parser.automaton
	make # need to sanitize otherwise
	make clean

itt:
	ocamlbuild $(OPTIONS) -no-links src/$(FILESRC)

createmli:
	ocamlc -i src/Kbd.ml > src/Kbd.mli

$(EXEC):
	@-ocamlbuild $(OPTIONS) -no-links src/$(FILESRC) > .log 
	@#echo ~~~~~full_log~~~~~~~
	@#cat .log
	@echo
	@#echo ~~~~~sanitized log~~
	-@./clean_make.sh .log
	-@#grep Warning .log > .log_warn || true
	-@#grep Error .log > .log_err || true

	@#@echo ~~~~~Warnings~~~~~~~
	@#cat .log_warn
	@#echo
	@#echo ~~~~~Errors~~~~~~~
	@#cat .log_err
	@-rm -f .log_err .log_warn .log
	@-cp _build/src/$(FILESRC) $(EXEC) 2>/dev/null

pdf: README.pdf

README.pdf: README.md
	pandoc -o README.pdf README.md

test: $(EXEC)
	./main test
