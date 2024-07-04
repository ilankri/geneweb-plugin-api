let volume_filename ~assets ~lang name =
  Filename.concat assets (Printf.sprintf "dico.%s.%s.bin~" name lang)
