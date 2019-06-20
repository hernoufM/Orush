CC=ocamlc
OBJECTS=file.cmo port.cmo moves.cmo solver.cmo fenetre.cmo main.cmo
all: orush

orush: $(OBJECTS)
	$(CC) -o orush graphics.cma $(OBJECTS)

main.cmo: main.ml
	$(CC) -c main.ml

fenetre.cmo: fenetre.cmi fenetre.ml
	$(CC) -c fenetre.ml

fenetre.cmi: fenetre.mli
	$(CC) -c fenetre.mli

solver.cmo: solver.cmi solver.ml
	$(CC) -c solver.ml

solver.cmi: solver.mli
	$(CC) -c solver.mli

moves.cmo: moves.cmi moves.ml
	$(CC) -c moves.ml

moves.cmi: moves.mli
	$(CC) -c moves.mli

port.cmo: port.cmi port.ml
	$(CC) -c port.ml

port.cmi: port.mli
	$(CC) -c port.mli

file.cmo: file.cmi file.ml
	$(CC) -c file.ml

file.cmi: file.mli
	$(CC) -c file.mli

clean:
	rm -rf *.cm[oi] orush
