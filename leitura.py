# #!/usr/bin/env python3
# #AUTH Diego Delmiro

import fileinput
import re
import sys
import os.path
from more_itertools import unique_everseen

copys = []
programas = []
arquivo_nomes_programas = "listaProgramas.txt"

if os.path.exists(arquivo_nomes_programas):
    try:
        nomes_programa = open(arquivo_nomes_programas, 'r')
        for nome in nomes_programa:
            nome_parte1 = nome[0:2]
            caminho = "F:\PRGNEW\%s\FONTES\%s" % (nome_parte1, nome)
            print(caminho)
            programas.append(caminho.rstrip())
    except EOFError:
        print("O %s Está em uso" % arquivo_nomes_programas)
else:
    print("Arquivo nao existe %s" % arquivo_nomes_programas)

if programas is not None:
    for programa in programas:
        try:
            arq = open(programa, 'r')
            arquivo = arq.readlines()
            arq.close()

            for lines in arquivo:
                if lines[lines.find("COPY") + 32:45] == 'EMI"':
                    copys.append(lines[lines.find("COPY") + 23:40])

            myfile = fileinput.FileInput(programa, inplace=1)

            for line in myfile:
                for copy in list(unique_everseen(copys)):
                    if line.__contains__(r"READ %s." % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END") and not line.__contains__("NEXT"):
                        line = re.sub(r"READ %s." % copy, r"READ %s WITH NO LOCK " % copy, line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s" % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END") and not line.__contains__("NEXT"):
                        line = re.sub(r"READ %s" % copy, r"READ %s WITH NO LOCK " % copy, line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s AT END" % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("NEXT"):
                        line = re.sub(r"READ %s AT END" % copy, r"READ %s WITH NO LOCK AT END" % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s NEXT." % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s NEXT." % copy, r"READ %s NEXT WITH NO LOCK " % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s PREVIOUS." % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("NEXT") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s PREVIOUS." % copy, r"READ %s PREVIOUS WITH NO LOCK " % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s NEXT" % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s NEXT " % copy, r"READ %s NEXT WITH NO LOCK " % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s PREVIOUS" % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("NEXT") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s PREVIOUS " % copy, r"READ %s PREVIOUS WITH NO LOCK " % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s KEY IS" % copy) and not line.__contains__(
                            "WITH NO LOCK") and line.__contains__(r"NEXT" % copy):
                        line = re.sub(r"READ %s KEY IS " % copy, r"READ %s WITH NO LOCK r'\n' KEY IS" % copy,
                                      line.rstrip()) + "\n"

                sys.stdout.write(line)

            myfile.close()
        except EOFError:
            print(EOFError)