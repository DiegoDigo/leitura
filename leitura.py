# #!/usr/bin/env python3

import fileinput
import re
from more_itertools import unique_everseen
import sys

print("Escutando alteraçoes")
print("*------------------*")
nome_arquivo = input("digite o nome do arquivo : ")

caminho = ""
nome_parte1 = nome_arquivo[0:2]
nome_parte2 = nome_arquivo[-1]

caminho = nome_arquivo + ".CBL"

arq = open(caminho, 'r')
arquivo = arq.readlines()
arq.close()
copys = []


def replace(TextAntigo, TextNovo, line):
    return re.sub(TextAntigo, TextNovo, line.rstrip())

for lines in arquivo:
    if lines[lines.find("COPY")+32:45] == 'EMI"':
        copys.append(lines[lines.find("COPY")+23:40])

myfile = fileinput.FileInput(caminho, inplace=1)

for line in myfile:
    for copy in list(unique_everseen(copys)):
        if line.__contains__(r"READ %s." % copy) and not line.__contains__("WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                and not line.__contains__("AT END") and not line.__contains__("NEXT"):
                line = re.sub(r"READ %s." % copy, r"READ %s WITH NO LOCK " % copy, line.rstrip())+ "\n"

        elif line.__contains__(r"READ %s" % copy) and not line.__contains__("WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                and not line.__contains__("AT END") and not line.__contains__("NEXT"):
                line = re.sub(r"READ %s" % copy, r"READ %s WITH NO LOCK " % copy, line.rstrip())+ "\n"

        elif line.__contains__(r"READ %s AT END" % copy) and not line.__contains__("WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                and not line.__contains__("NEXT"):
                line = re.sub(r"READ %s AT END " % copy, r"READ %s WITH NO LOCK AT END" % copy, line.rstrip())+ "\n"

        elif line.__contains__(r"READ %s NEXT." % copy) and not line.__contains__("WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                and not line.__contains__("AT END"):
                line = re.sub(r"READ %s NEXT." % copy, r"READ %s NEXT WITH NO LOCK " % copy, line.rstrip())+ "\n"

        elif line.__contains__(r"READ %s PREVIOUS." % copy) and not line.__contains__("WITH NO LOCK") and not line.__contains__("NEXT") \
                and not line.__contains__("AT END"):
                line = re.sub(r"READ %s PREVIOUS." % copy, r"READ %s PREVIOUS WITH NO LOCK " % copy, line.rstrip())+ "\n"

        elif line.__contains__(r"READ %s NEXT" % copy) and not line.__contains__("WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                and not line.__contains__("AT END"):
                line = re.sub(r"READ %s NEXT " % copy, r"READ %s NEXT WITH NO LOCK " % copy, line.rstrip())+ "\n"

        elif line.__contains__(r"READ %s PREVIOUS" % copy) and not line.__contains__("WITH NO LOCK") and not line.__contains__("NEXT") \
                and not line.__contains__("AT END"):
                line = re.sub(r"READ %s PREVIOUS " % copy, r"READ %s PREVIOUS WITH NO LOCK " % copy, line.rstrip()) + "\n"

        elif line.__contains__(r"READ %s KEY IS" % copy) and not line.__contains__("WITH NO LOCK") and line.__contains__(r"NEXT" % copy):
                line = re.sub(r"READ %s KEY IS " % copy, r"READ %s WITH NO LOCK '\n' KEY IS" % copy, line.rstrip()) + "\n"

    sys.stdout.write(line)

myfile.close()






