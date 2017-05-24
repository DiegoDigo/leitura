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
arquivoInicial = []
arquivo_nomes_programas = ""
nome_programa = ""
ano = 0
mes = 0
versao = 0
versaoAquivo = ""
versaoAlterada = ""
editado, lista, subir, movePrgori = False, False, False, False



def salvarProgramasAlterados(programa=None):
    try:
        arquivoProgramasAlterado = open("programasAlterados.txt", 'a+')
        arquivoProgramasAlterado.writelines(programa[20:28] + " " + datetime.now().strftime("%d-%m-%Y %H:%M:%S ")
                                            + "\n")
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
    if float(versao) > 0:
        novaVersao = int(versao) + 1

    return formataMesVersao(anoAtual, mesAtual, novaVersao)


def mudarVersao(editado=False, programa=None, subir=False):
    if editado and subir:
        temporario = criarTemporario(programa)
        temp = lerTemporario(temporario)
        os.system("del %s" % programa)
        arquivoFinal = open(programa, 'w+', encoding="iso-8859-1")
        for lines in temp:
            lines = str(lines)
            if lines.__contains__("77 WTPGM-VERSAO"):
                ano = lines[lines.find("VALUE") + 7:60].split(".")[0]
                mes = lines[lines.find("VALUE") + 7:60].split(".")[1]
                versao = lines[lines.find("VALUE") + 7:60].split(".")[2]
                if versao[-1::] == '"':
                    versao = versao.replace('"', '')
                versaoAquivo = '"%s.%s.%s"' % (ano, mes, versao)
                versaoAlterada = verificaVersao(ano=ano, mes=mes, versao=versao)
                lines = re.sub(versaoAquivo, versaoAlterada, lines.rstrip()) + "\n"
            arquivoFinal.write(str(lines))
        arquivoFinal.close()
        os.system("del %s" % temporario)
        salvarProgramasAlterados(programa)
        print(u"O Programa " + programa + " teve a versão alterada de " + versaoAquivo + " para " + versaoAlterada)
    elif editado:
        salvarProgramasAlterados(programa)
    else:
        print(u"O " + programa + " não teve alteração de versão")


def verificaDiretorio(nome=None):
    nome_parte1 = nome[0:2]
    nome = nome.replace("\n", "")
    caminho = "F:\PRGNEW\%s\FONTES\%s" % (nome_parte1, nome)
    if os.path.isfile(caminho):
        programas.append(caminho.rstrip())
        return False
    else:
        caminhoOri = "F:\PRGORI\%s\FONTES\%s" % (nome_parte1, nome)
        caminhoPRGNEW = "F:\PRGNEW\%s\FONTES\\" % nome_parte1
        if os.path.isfile(caminhoOri):
            os.system("move %s %s " % (caminhoOri, caminhoPRGNEW))
            programas.append(caminho.rstrip())
            return True
        else:
            print("O %s nao existe no PRGORI" % caminhoOri)


def criarTemporario(programa=None):
    caminhoTemp = "%s.bak" % programa
    os.system("copy {0} {1}".format(programa, caminhoTemp))
    with open(caminhoTemp, 'w+', encoding="iso-8859-1") as arquivo_temp:
        for linhas in fileinput.FileInput(programa, openhook=fileinput.hook_encoded("iso-8859-1")):
            arquivo_temp.write(str(linhas))
        fileinput.close()
        arquivo_temp.close()
    return caminhoTemp


def lerTemporario(temporario=None):
    arqTemp = open(temporario, 'r', encoding="iso-8859-1")
    arquivoTemp = arqTemp.readlines()
    arqTemp.close()
    return arquivoTemp

# arquivo_programa_lista = str(sys.argv[1])
# resposta = str(sys.argv[2])
arquivo_programa_lista = "VDFANACM.CBL"
resposta = "S"

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
            movePrgori = verificaDiretorio(nome=nome)
    except EOFError:
        print(u"O %s Está em uso" % arquivo_nomes_programas)
else:
    movePrgori = verificaDiretorio(nome=nome_programa)

if len(programas) > 0 and programas is not None:
    for programa in programas:
        print(u"ALTERANDO O PROGRAMA %s" % programa)
        try:
            arq = open(programa, 'rb')
            arquivo = arq.readlines()
            arq.close()

            for lines in arquivo:
                lines = str(lines)
                if lines.__contains__("COPY") and lines.__contains__(".EMI") and not lines.__contains__(".EMIO"):
                    copys.append(lines.rstrip()[38:46])

            temporario = criarTemporario(programa)

            os.system("del %s" % programa)

            arquivoFinal = open(programa, 'w+', encoding="iso-8859-1")

            for line in lerTemporario(temporario):
                line = str(line)
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
                            line.__contains__(r"NEXT %s" % copy):
                        textoEditado = r'READ %s WITH NO LOCK %s KEY IS' % (copy, os.linesep)
                        line = re.sub(r"READ %s KEY IS" % copy, textoEditado, line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s PREVIOUS AT END" % copy) and not line.__contains__("WITH NO LOCK")\
                            and line.__contains__(r"NEXT %s" % copy):
                        textoEditado = r'READ %s PREVIOUS WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s PREVIOUS AT END" % copy, textoEditado, line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s PREVIOUS, AT END" % copy) and \
                            not line.__contains__("WITH NO LOCK") and line.__contains__(r"NEXT %s" % copy):
                        textoEditado = r'READ %s PREVIOUS, WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s PREVIOUS, AT END" % copy, textoEditado, line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s NEXT AT END" % copy) and not line.__contains__("WITH NO LOCK") \
                            and line.__contains__(r"NEXT %s" % copy):
                        textoEditado = r'READ %s NEXT WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s NEXT AT END" % copy, textoEditado, line.rstrip()) + "\n"
                        editado = True

                    elif line.__contains__(r"READ %s NEXT, AT END" % copy) and not line.__contains__("WITH NO LOCK") \
                            and line.__contains__(r"NEXT %s" % copy):
                        textoEditado = r'READ %s NEXT, WITH NO LOCK AT END' % (copy, os.linesep)
                        line = re.sub(r"READ %s NEXT, AT END" % copy, textoEditado, line.rstrip()) + "\n"
                        editado = True

                arquivoFinal.write(str(line))

            arquivoFinal.close()
            print(u"TERMINO DA ALTERAÇÃO DO PROGRAMA %s" % programa)
            print()
        except EOFError:
            print(EOFError)

        mudarVersao(editado, programa, subir)

        os.system("move %s %s " % (programa + ".bak", "F:\PRGOLD\%s\FONTES\\" % programa[20:22]))

        if movePrgori:
            os.system("move %s %s " % (programa, "F:\PRGORI\%s\FONTES\\" % programa[20:22]))

else:
    print(u"O %s esta vazio por favor verificar" % arquivo_nomes_programas)
