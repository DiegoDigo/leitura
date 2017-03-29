# #!/usr/bin/env python3
# -*- encoding: utf-8 -*-
# AUTHOR diego.delmiro
# DATA 28/03/2017

import fileinput
import re
import sys
import os.path
from more_itertools import unique_everseen

copys = []
programas = []
arquivo_nomes_programas = "listaProgramas.txt"

for arquivo in sys.argv[1:]:
    if arquivo[len(arquivo)-4:len(arquivo)+4] == ".txt" or arquivo[len(arquivo)-4:len(arquivo)+4] == ".TXT":
        print(arquivo[0:len(arquivo)-4])


if sys.argv[1:] ==

if os.path.exists(arquivo_nomes_programas):
    try:
        nomes_programa = open(arquivo_nomes_programas, 'r')
        for nome in nomes_programa:
            nome_parte1 = nome[0:2]
            caminho = "F:\PRGNEW\%s\FONTES\%s" % (nome_parte1, nome)
            programas.append(caminho.rstrip())
    except EOFError:
        print("O %s Está em uso" % arquivo_nomes_programas)
else:
    print("Arquivo nao existe %s , Por favor criar o arquivo" % arquivo_nomes_programas)

if len(programas) > 0 and programas is not None:
    for programa in programas:
        print("ALTERANDO O PROGRAMA %s" %programa)
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
                        line = re.sub(r"READ %s." % copy, r"READ %s WITH NO LOCK." % copy, line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s" % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END") and not line.__contains__("NEXT"):
                        line = re.sub(r"READ %s" % copy, r"READ %s WITH NO LOCK " % copy, line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s AT END" % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("NEXT"):
                        line = re.sub(r"READ %s AT END" % copy, r"READ %s WITH NO LOCK AT END" % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s AT END." % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("NEXT"):
                        line = re.sub(r"READ %s AT END." % copy, r"READ %s WITH NO LOCK AT END." % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s NEXT." % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s NEXT." % copy, r"READ %s NEXT WITH NO LOCK." % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s NEXT," % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s NEXT," % copy, r"READ %s NEXT, WITH NO LOCK" % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s NEXT" % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s NEXT" % copy, r"READ %s NEXT WITH NO LOCK" % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s PREVIOUS." % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("NEXT") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s PREVIOUS." % copy, r"READ %s PREVIOUS WITH NO LOCK." % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s PREVIOUS," % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("NEXT") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s PREVIOUS," % copy, r"READ %s PREVIOUS, WITH NO LOCK" % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s PREVIOUS" % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("NEXT") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s PREVIOUS" % copy, r"READ %s PREVIOUS WITH NO LOCK" % copy,
                                      line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s KEY IS" % copy) and not line.__contains__(
                            "WITH NO LOCK") and line.__contains__(r"NEXT" % copy):
                        textoEditado = r'READ %s WITH NO LOCK %s KEY IS' % (copy, os.linesep)
                        line = re.sub(r"READ %s KEY IS" % copy, textoEditado, line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s PREVIOUS AT END" % copy) and not line.__contains__(
                            "WITH NO LOCK") and line.__contains__(r"NEXT" % copy):
                        textoEditado = r'READ %s PREVIOUS WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s PREVIOUS AT END" % copy, textoEditado, line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s PREVIOUS, AT END" % copy) and not line.__contains__(
                            "WITH NO LOCK") and line.__contains__(r"NEXT" % copy):
                        textoEditado = r'READ %s PREVIOUS, WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s PREVIOUS, AT END" % copy, textoEditado, line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s NEXT AT END" % copy) and not line.__contains__(
                            "WITH NO LOCK") and line.__contains__(r"NEXT" % copy):
                        textoEditado = r'READ %s NEXT WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s NEXT AT END" % copy, textoEditado, line.rstrip()) + "\n"

                    elif line.__contains__(r"READ %s NEXT, AT END" % copy) and not line.__contains__(
                            "WITH NO LOCK") and line.__contains__(r"NEXT" % copy):
                        textoEditado = r'READ %s NEXT, WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s NEXT, AT END" % copy, textoEditado, line.rstrip()) + "\n"

                sys.stdout.write(line)

            myfile.close()
            print("TERMINO DA ALTERAÇÃO DO PROGRAMA %s" % programa)
            print()
        except EOFError:
            print(EOFError)
else:
    print("O %s esta vazio por favor verificar" %arquivo_nomes_programas)
