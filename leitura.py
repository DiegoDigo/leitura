#!/usr/bin/env python3


print("Escutando alteraçoes")
print("*------------------*")
nome_arquivo = input("digite o nome do arquivo : ")

caminho = ""
nome_parte1 = nome_arquivo[0:2]
nome_parte2 = nome_arquivo[-1]

# if nome_parte2 == 'A' or nome_parte2 == 'E' or nome_parte2 == 'M' or nome_parte2 == 'P' or nome_parte2 == 'I':
#     caminho = "F:\PRGNEW\%s\FONTES\%s" % (nome_parte1, nome_arquivo+".CBL")
#     print(caminho)

caminho = nome_arquivo + ".CBL"


def trocarREAD(texto,novoTexto):
    arq.write(lines.replace(texto, novoTexto))

# try:
listaCOPY = []
arq = open(caminho, 'r+')
# arquivo_novo = open(caminho, 'a+')
for lines in arq.readlines():
    # if lines.count("COPY"):
    #    # if lines[lines.find("COPY")+32:45] == 'EMI"':
    #    #     print(lines[lines.find("COPY")+32:27])
    #    #     print(lines)

    if lines.count("READ"):
        frase = lines[lines.find("READ"):lines.find("READ")+14]
        if frase[-1] == ".":
            trocarREAD(".", " WITH NO LOCK")

       # arq.write(lines)

arq.close()

# except:
#     print("arquivo nao encontrado !")


