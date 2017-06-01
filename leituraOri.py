# #!/usr/bin/env python3
# -*- encoding: utf-8 -*-
# CREATION DATE 31/05/2017

from __future__ import unicode_literals
from more_itertools import unique_everseen
from datetime import datetime
import os
import re
import sys

nomeProgramaOuLista = str(sys.argv[1])
subirVersao = str(sys.argv[2])

# nomeProgramaOuLista = "c:\\testeRodar.txt"
# subirVersao = "s"


def moverArquivos(nomePrograma=None):
    nomePrograma = nomePrograma.replace("\n", "")
    caminhoArquivoOri = "F:\PRGORI\{0}\FONTES\{1}".format(nomePrograma[0:2], nomePrograma)
    caminhoArquivoNew = "F:\PRGNEW\{0}\FONTES\{1}".format(nomePrograma[0:2], nomePrograma)
    if os.path.isfile(caminhoArquivoOri) and not os.path.isfile(caminhoArquivoNew):
        os.system("COPY {0} {1}".format(caminhoArquivoOri, caminhoArquivoNew))
        os.system("REN {0} {1}{2}.DDD".format(caminhoArquivoOri, nomePrograma[0:8], nomePrograma[-3::]))
        return caminhoArquivoNew


def verExtencao(nomePrograma=None):
    listaArquivos = []
    if nomePrograma[-3::] == "txt" or nomePrograma[-3::] == "TXT":
        for arquivos in lerArquivo(nomePrograma):
            listaArquivos.append(moverArquivos(arquivos))
    else:
        listaArquivos.append(moverArquivos(nomeProgramaOuLista))
    return listaArquivos


def lerArquivo(nomePrograma=None):
    with open(nomePrograma, 'r', encoding="iso-8859-1") as arquivo:
        aq = arquivo.readlines()
        arquivo.close()
    return aq


def pegarArquivosEmi(nomePrograma=None):
    programaEmis = []
    for linha in lerArquivo(nomePrograma):
        linha = str(linha)
        if linha.__contains__("COPY") and linha.__contains__(".EMI") \
            and not linha.rstrip().__contains__(".EMIO"):
            programaEmis.append(linha[32:40])
    return programaEmis


def criarArquivoTemporario(nomePrograma=None):
    arquivoEntrada = lerArquivo(nomePrograma)
    with open('{0}.bak'.format(nomePrograma), 'w+', encoding="iso-8859-1") as arquivoSaida:
        for linha in arquivoEntrada:
            arquivoSaida.write(str(linha))
        arquivoSaida.close()
    os.system("move {0} {1}".format(nomePrograma+".bak", "F:\\PRGOLD\\{0}\\FONTES\\".format(nomePrograma[20:22])))


def editarLinhas(arquivo=None, nomePrograma=None, copys=None, subirVersao=None):
    editado = False
    arquivoFinal = open(nomePrograma, 'w+', encoding="iso-8859-1")

    for line in arquivo:
        line = str(line)

        if line.__contains__("77 WTPGM-VERSAO") and subirVersao == "S":
            versaoAnterior = line.split()[5][1:10]
            versaoFinal = mudarVersao(int(line.split()[5][1:3]), int(line.split()[5][4:6]),
                                      int(line.split()[5][7:10]))
            line = re.sub('"{0}"'.format(versaoAnterior), '"{0}"'.format(versaoFinal), line.rstrip()) + "\n"

        for copy in list(unique_everseen(copys)):

            if line.__contains__(r"READ %s." % copy) and not line.__contains__("WITH NO LOCK") and \
                    not line.__contains__("PREVIOUS") \
                    and not line.__contains__("AT END") and not line.__contains__("NEXT"):
                line = re.sub(r"READ %s." % copy, r"READ %s WITH NO LOCK." % copy, line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s" % copy) and not line.__contains__(
                    "WITH NO LOCK") and not line.__contains__("PREVIOUS") \
                    and not line.__contains__("AT END") and not line.__contains__("NEXT"):
                line = re.sub(r"READ %s" % copy, r"READ %s WITH NO LOCK " % copy, line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s AT END" % copy) and not line.__contains__("WITH NO LOCK") and \
                    not line.__contains__("PREVIOUS") \
                    and not line.__contains__("NEXT"):
                line = re.sub(r"READ %s AT END" % copy, r"READ %s WITH NO LOCK AT END" % copy,
                              line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s AT END." % copy) and not line.__contains__("WITH NO LOCK") and \
                    not line.__contains__("PREVIOUS") \
                    and not line.__contains__("NEXT"):
                line = re.sub(r"READ %s AT END." % copy, r"READ %s WITH NO LOCK AT END." % copy,
                              line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s NEXT." % copy) and not line.__contains__("WITH NO LOCK") and \
                    not line.__contains__("PREVIOUS") \
                    and not line.__contains__("AT END"):
                line = re.sub(r"READ %s NEXT." % copy, r"READ %s NEXT WITH NO LOCK." % copy,
                              line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s NEXT," % copy) and not line.__contains__("WITH NO LOCK") and \
                    not line.__contains__("PREVIOUS") \
                    and not line.__contains__("AT END"):
                line = re.sub(r"READ %s NEXT," % copy, r"READ %s NEXT, WITH NO LOCK" % copy,
                              line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s NEXT" % copy) and not line.__contains__("WITH NO LOCK") and \
                    not line.__contains__("PREVIOUS") \
                    and not line.__contains__("AT END"):
                line = re.sub(r"READ %s NEXT" % copy, r"READ %s NEXT WITH NO LOCK" % copy,
                              line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s PREVIOUS." % copy) and not line.__contains__("WITH NO LOCK") and \
                    not line.__contains__("NEXT") \
                    and not line.__contains__("AT END"):
                line = re.sub(r"READ %s PREVIOUS." % copy, r"READ %s PREVIOUS WITH NO LOCK." % copy,
                              line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s PREVIOUS," % copy) and not line.__contains__("WITH NO LOCK") and \
                    not line.__contains__("NEXT") \
                    and not line.__contains__("AT END"):
                line = re.sub(r"READ %s PREVIOUS," % copy, r"READ %s PREVIOUS, WITH NO LOCK" % copy,
                              line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s PREVIOUS" % copy) and not line.__contains__("WITH NO LOCK") and \
                    not line.__contains__("NEXT") \
                    and not line.__contains__("AT END"):
                line = re.sub(r"READ %s PREVIOUS" % copy, r"READ %s PREVIOUS WITH NO LOCK" % copy,
                              line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s KEY IS" % copy) and not line.__contains__("WITH NO LOCK") and \
                    line.__contains__(r"NEXT %s" % copy):
                textoEditado = r'READ %s WITH NO LOCK %s KEY IS' % (copy, os.linesep)
                line = re.sub(r"READ %s KEY IS" % copy, textoEditado, line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s PREVIOUS AT END" % copy) and not line.__contains__("WITH NO LOCK") \
                    and line.__contains__(r"NEXT %s" % copy):
                textoEditado = r'READ %s PREVIOUS WITH NO LOCK AT END' % (copy, os.linesep)
                line = re.sub(r"READ %s PREVIOUS AT END" % copy, textoEditado, line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s PREVIOUS, AT END" % copy) and \
                    not line.__contains__("WITH NO LOCK") and line.__contains__(r"NEXT %s" % copy):
                textoEditado = r'READ %s PREVIOUS, WITH NO LOCK AT END' % (copy, os.linesep)
                line = re.sub(r"READ %s PREVIOUS, AT END" % copy, textoEditado, line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s NEXT AT END" % copy) and not line.__contains__("WITH NO LOCK") \
                    and line.__contains__(r"NEXT %s" % copy):
                textoEditado = r'READ %s NEXT WITH NO LOCK AT END' % (copy, os.linesep)
                line = re.sub(r"READ %s NEXT AT END" % copy, textoEditado, line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s NEXT, AT END" % copy) and not line.__contains__("WITH NO LOCK") \
                    and line.__contains__(r"NEXT %s" % copy):
                textoEditado = r'READ %s NEXT, WITH NO LOCK AT END' % (copy, os.linesep)
                line = re.sub(r"READ %s NEXT, AT END" % copy, textoEditado, line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s NEXT RECORD" % copy) and not line.__contains__("WITH NO LOCK") \
                    and line.__contains__(r"NEXT %s" % copy):
                textoEditado = r'READ %s NEXT RECORD WITH NO LOCK AT END' % (copy, os.linesep)
                line = re.sub(r"READ %s NEXT, AT END" % copy, textoEditado, line.rstrip()) + "\n"
                editado = True

            if line.__contains__(r"READ %s NEXT, RECORD" % copy) and not line.__contains__("WITH NO LOCK") \
                    and line.__contains__(r"NEXT %s" % copy):
                textoEditado = r'READ %s NEXT, RECORD WITH NO LOCK AT END' % (copy, os.linesep)
                line = re.sub(r"READ %s NEXT, RECORD %s" % copy, textoEditado, line.rstrip()) + "\n"
                editado = True

        arquivoFinal.write(str(line))
    arquivoFinal.close()
    print("versão anterior : {0} versao atual : {1}". format(versaoAnterior, versaoFinal))
    return editado


def mudarVersao(ano=None, mes=None, versao=None):
    if isinstance(ano, int) and ano < int(str(datetime.today().year)[-2::]):
        ano = int(str(datetime.today().year)[-2::])
    if isinstance(mes, int):
        if len(str(mes)) == 1:
            mes = "0{0}".format(datetime.today().month)
        else:
            mes = datetime.today().month
    if isinstance(versao, int) and versao > 0:
        if len(str(versao)) == 1:
            versao = "00" + str(versao + 1)
        elif len(str(versao)) == 2:
            versao = "0" + str(versao + 1)
        else:
            versao = versao + 1
    return "%s.%s.%s" % (str(ano), str(mes), str(versao))


listaProgramas = verExtencao(nomeProgramaOuLista)


for programa in listaProgramas:
    if programa is not None:
        print("Editando o programa {}".format(programa))
        criarArquivoTemporario(programa)
        listaCopys = pegarArquivosEmi(programa)
        arquivo = lerArquivo(programa)
        try:
            editarLinhas(arquivo, programa, listaCopys, subirVersao.upper())
        except EOFError:
            raise EOFError("Erro ao editar o arquivo")



