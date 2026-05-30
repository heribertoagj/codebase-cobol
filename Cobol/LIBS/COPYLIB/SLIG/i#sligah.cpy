      *** AREA DE COMUNICACAO - IMS
      *** MENU IMS SLIG1990
      ***===========================================================***
      *** NOME INC                                     LENGTH=0218  ***
      *** I#SLIGAH                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICAO DO PROGRAMA SLIG1990         ***
      ***            COM OS PROGRAMAS CHAMADOS                      ***
      ***                                                           ***
      ***            BASE DA I#SLIGA1 - INCLUSÃO DOS CAMPOS         ***
      ***            FILIAL E CONTROLE                              ***
      ***            BANCO, AGENCIA E CONTA DE PESQUISA             ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *
       01  SLIGAH-AREA.
           03 SLIGAH-LL                    PIC S9(004) COMP VALUE +218.
           03 SLIGAH-ZZ                    PIC S9(004) COMP VALUE +0.
           03 SLIGAH-TRANCODE              PIC  X(015).
           03 SLIGAH-SENHAS                PIC  X(037).
           03 SLIGAH-COMANDO               PIC  X(008).
           03 SLIGAH-PGM-CHAMADOR          PIC  X(008).
           03 SLIGAH-DADOS-ENVIADOS.
              05 SLIGAH-PERFIL-USUARIO     PIC  X(002).
              05 SLIGAH-CBCO               PIC  9(003).
              05 SLIGAH-CDEPDC             PIC  9(005).
              05 SLIGAH-CFUNC-BDSCO        PIC  9(009).
              05 SLIGAH-DADOS-PESQUISA.
                 10 SLIGAH-CBCOP           PIC  9(003).
                 10 SLIGAH-CDEPDCP         PIC  9(005).
CNPJ7C           10 SLIGAH-CCGC-CPF        PIC  X(009).
CNPJ7C           05 SLIGAH-CCGC-CPF-FILIAL PIC  X(005).
                 05 SLIGAH-CCGC-CPF-CONTR  PIC  9(002).
                 05 SLIGAH-TP-FORMATO      PIC  X(001).
                 05 SLIGAH-CBCO-CLI-SAC    PIC  9(003).
                 05 SLIGAH-CAG-CLI-SAC     PIC  9(005).
                 05 SLIGAH-CCTA-CLI-SAC    PIC  9(013).
           03 SLIGAH-DADOS-RETORNADOS.
              05 SLIGAH-COD-RETORNO        PIC  9(002).
              05 SLIGAH-MENSAGEM           PIC  X(079).
