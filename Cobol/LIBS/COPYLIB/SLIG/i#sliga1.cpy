      *** AREA DE COMUNICACAO - IMS
      *** MENU IMS SLIG1990
      ***===========================================================***
      *** NOME INC                                     LENGTH=0189  ***
      *** I#SLIGA1                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICAO DO PROGRAMA SLIG1990         ***
      ***            COM OS PROGRAMAS CHAMADOS                      ***
      ***===========================================================***
      *
       01  SLIGA1-AREA.
           03 SLIGA1-LL                    PIC S9(004) COMP VALUE +189.
           03 SLIGA1-ZZ                    PIC S9(004) COMP VALUE +0.
           03 SLIGA1-TRANCODE              PIC  X(015).
           03 SLIGA1-SENHAS                PIC  X(037).
           03 SLIGA1-COMANDO               PIC  X(008).
           03 SLIGA1-PGM-CHAMADOR          PIC  X(008).
           03 SLIGA1-DADOS-ENVIADOS.
              05 SLIGA1-PERFIL-USUARIO     PIC  X(002).
              05 SLIGA1-CBCO               PIC  9(003).
              05 SLIGA1-CDEPDC             PIC  9(005).
              05 SLIGA1-CFUNC-BDSCO        PIC  9(009).
              05 SLIGA1-DADOS-PESQUISA.
                 10 SLIGA1-CBCOP           PIC  9(003).
                 10 SLIGA1-CDEPDCP         PIC  9(005).
                 10 SLIGA1-CCGC-CPF        PIC  X(009).
           03 SLIGA1-DADOS-RETORNADOS.
              05 SLIGA1-COD-RETORNO        PIC  9(002).
              05 SLIGA1-MENSAGEM           PIC  X(079).
