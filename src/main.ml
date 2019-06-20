if(Array.length Sys.argv)=1
then print_endline "Passer avec la ligne du commande le path vers fichier du representation textuels du boats!"
else
  let fich = open_in Sys.argv.(1) in
  Fenetre.start (Port.input_state fich);
  close_in fich;
