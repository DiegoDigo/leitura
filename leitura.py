# #!/usr/bin/env python3
# -*- encoding: utf-8 -*-
# DATA 28/03/2017
# EDITADO 30/03/2017

from __future__ import unicode_literals
from datetime import datetime
from more_itertools import unique_everseen
import fileinput
import re
import sys
import os.path

__autor__ = u"Diego Delmiro"

copys = []
programas = []
arquivo_nomes_programas = ""
nome_programa = ""
ano = 0
mes = 0
versao = 0
versaoAquivo = ""
versaoAlterada = ""
editado, lista, subir = False, False, False


def salvarProgramasAlterados(programa, ):
    try:
        arquivoProgramasAlterado = open("programasAlterados.txt", 'a+')
        arquivoProgramasAlterado.writelines(programa[20:28] + datetime.now().strftime("%d-%m-%Y %H:%M:%S "))
        arquivoProgramasAlterado.close()
    except EOFError:
        print(u"Arquivo esta em uso")


def formataMesVersao(ano=None, mes=None, versao=None):
    if len(str(mes)) == 1:
        mes = "0" + str(mes)
    if len(str(versao)) == 1:
        versao = "00" + str(versao)
    if len(str(versao)) == 2:
        versao = "0" + str(versao)
    return '"%s.%s.%s"' % (ano, mes, versao)


def verificaVersao(ano=None, mes=None, versao=None):
    if ano <= str(datetime.now().year)[-2::] or ano >= str(datetime.now().year)[-2::]:
        anoAtual = str(datetime.now().year)[-2::]
    if int(mes) < datetime.now().month or int(mes) >= datetime.now().month:
        mesAtual = datetime.now().month
    if int(versao) > 0:
        novaVersao = int(versao) + 1

    return formataMesVersao(anoAtual, mesAtual, novaVersao)


def mudarVersao(editado, programa, subir):
    if editado and subir:
        myfile = fileinput.FileInput(programa, inplace=1)
        for lines in myfile:
            if lines.__contains__("77 WTPGM-VERSAO"):
                ano = lines[lines.find("VALUE") + 7:60].split(".")[0]
                mes = lines[lines.find("VALUE") + 7:60].split(".")[1]
                versao = lines[lines.find("VALUE") + 7:60].split(".")[2]
                versaoAquivo = '"%s.%s.%s"' % (ano, mes, versao)
                versaoAlterada = verificaVersao(ano=ano, mes=mes, versao=versao)
                lines = re.sub(versaoAquivo, versaoAlterada, lines.rstrip()) + "\n"

            sys.stdout.write(lines)
        myfile.close()
        salvarProgramasAlterados(programa)
        print(u"O Programa " + programa + " teve a versão alterada de " + versaoAquivo + " para " + versaoAlterada)
    elif editado:
        salvarProgramasAlterados(programa)
    else:
        print(u"O " + programa + " não teve alteração de versão")


arquivo_programa_lista = str(sys.argv[1])
resposta = str(sys.argv[2])

if str(arquivo_programa_lista)[len(arquivo_programa_lista) - 4:len(arquivo_programa_lista) + 4] == ".txt" \
        or arquivo_programa_lista[len(arquivo_programa_lista) - 4:len(arquivo_programa_lista) + 4] == ".TXT":
    lista = True
    arquivo_nomes_programas = arquivo_programa_lista

elif arquivo_programa_lista[len(arquivo_programa_lista) - 4:len(arquivo_programa_lista) + 4] == ".CBL" \
        or arquivo_programa_lista[len(arquivo_programa_lista) - 4:len(arquivo_programa_lista) + 4] == ".CCA":
    nome_programa = arquivo_programa_lista

else:
    print(u"Formato invalido , permitido somente .CBL, .CCA e .TXT")

if resposta == "s" or resposta == "S":
    subir = True

if lista:
    try:
        nomes_programa = open(arquivo_nomes_programas, 'r')
        for nome in nomes_programa:
            nome_parte1 = nome[0:2]
            caminho = "F:\PRGNEW\%s\FONTES\%s" % (nome_parte1, nome)
            programas.append(caminho.rstrip())
    except EOFError:
        print(u"O %s Está em uso" % arquivo_nomes_programas)
else:
    nome_parte1 = nome_programa[0:2]
    caminho = "F:\PRGNEW\%s\FONTES\%s" % (nome_parte1, nome_programa)
    programas.append(caminho)

if len(programas) > 0 and programas is not None:
    for programa in programas:
        print(u"ALTERANDO O PROGRAMA %s" % programa)
        try:
            arq = open(programa, 'r')
            arquivo = arq.readlines()
            arq.close()

            for lines in arquivo:
                if lines[lines.find("COPY") + 32:45] == 'EMI"':
                    copys.append(lines[lines.find("COPY") + 23:40])

            myfile = fileinput.FileInput(programa, inplace=1, backup=".bak")

            for line in myfile:
                for copy in list(unique_everseen(copys)):

                    if line.__contains__(r"READ %s." % copy) and not line.__contains__("WITH NO LOCK") and \
                            not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END") and not line.__contains__("NEXT"):
                        line = re.sub(r"READ %s." % copy, r"READ %s WITH NO LOCK." % copy, line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s" % copy) and not line.__contains__(
                            "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END") and not line.__contains__("NEXT"):
                        line = re.sub(r"READ %s" % copy, r"READ %s WITH NO LOCK " % copy, line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s AT END" % copy) and not line.__contains__("WITH NO LOCK") and \
                            not line.__contains__("PREVIOUS") \
                            and not line.__contains__("NEXT"):
                        line = re.sub(r"READ %s AT END" % copy, r"READ %s WITH NO LOCK AT END" % copy,
                                      line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s AT END." % copy) and not line.__contains__("WITH NO LOCK") and \
                            not line.__contains__("PREVIOUS") \
                            and not line.__contains__("NEXT"):
                        line = re.sub(r"READ %s AT END." % copy, r"READ %s WITH NO LOCK AT END." % copy,
                                      line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s NEXT." % copy) and not line.__contains__("WITH NO LOCK") and \
                            not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s NEXT." % copy, r"READ %s NEXT WITH NO LOCK." % copy,
                                      line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s NEXT," % copy) and not line.__contains__("WITH NO LOCK") and \
                            not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s NEXT," % copy, r"READ %s NEXT, WITH NO LOCK" % copy,
                                      line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s NEXT" % copy) and not line.__contains__("WITH NO LOCK") and \
                            not line.__contains__("PREVIOUS") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s NEXT" % copy, r"READ %s NEXT WITH NO LOCK" % copy,
                                      line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s PREVIOUS." % copy) and not line.__contains__("WITH NO LOCK") and \
                            not line.__contains__("NEXT") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s PREVIOUS." % copy, r"READ %s PREVIOUS WITH NO LOCK." % copy,
                                      line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s PREVIOUS," % copy) and not line.__contains__("WITH NO LOCK") and \
                            not line.__contains__("NEXT") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s PREVIOUS," % copy, r"READ %s PREVIOUS, WITH NO LOCK" % copy,
                                      line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s PREVIOUS" % copy) and not line.__contains__("WITH NO LOCK") and \
                            not line.__contains__("NEXT") \
                            and not line.__contains__("AT END"):
                        line = re.sub(r"READ %s PREVIOUS" % copy, r"READ %s PREVIOUS WITH NO LOCK" % copy,
                                      line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s KEY IS" % copy) and not line.__contains__("WITH NO LOCK") and \
                            line.__contains__(r"NEXT" % copy):
                        textoEditado = r'READ %s WITH NO LOCK %s KEY IS' % (copy, os.linesep)
                        line = re.sub(r"READ %s KEY IS" % copy, textoEditado, line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s PREVIOUS AT END" % copy) and not line.__contains__("WITH NO LOCK")\
                            and line.__contains__(r"NEXT" % copy):
                        textoEditado = r'READ %s PREVIOUS WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s PREVIOUS AT END" % copy, textoEditado, line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s PREVIOUS, AT END" % copy) and \
                            not line.__contains__("WITH NO LOCK") and line.__contains__(r"NEXT" % copy):
                        textoEditado = r'READ %s PREVIOUS, WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s PREVIOUS, AT END" % copy, textoEditado, line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s NEXT AT END" % copy) and not line.__contains__("WITH NO LOCK") \
                            and line.__contains__(r"NEXT" % copy):
                        textoEditado = r'READ %s NEXT WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s NEXT AT END" % copy, textoEditado, line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s NEXT, AT END" % copy) and not line.__contains__("WITH NO LOCK") \
                            and line.__contains__(r"NEXT" % copy):
                        textoEditado = r'READ %s NEXT, WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s NEXT, AT END" % copy, textoEditado, line.rstrip()) + "\n"
                        editado = True

                sys.stdout.write(line)

            myfile.close()
            print(u"TERMINO DA ALTERAÇÃO DO PROGRAMA %s" % programa)
            print()
        except EOFError:
            print(EOFError)

    mudarVersao(editado, programa, subir)

else:
    print(u"O %s esta vazio por favor verificar" % arquivo_nomes_programas)
