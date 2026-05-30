      ***===========================================================***
      *** NOME INC                                     LENGTH=0040  ***
      *** I#SLIGF1                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO DE GERACAO DE CCC                       **
      **                                                             **
      **  SLIGF1-H-COD-REG             - REGISTRO HEADER = 0         **
      **  SLIGF1-H-NOM-ARQ             - DESCRICAO DO ARQUIVO        **
      **  SLIGF1-H-DATA-GER            - DATA DE GERACAO DO ARQUIVO  **
      **  SLIGF1-D-TPO-REG             - REGISTRO DETALHE = 1        **
      **  SLIGF1-D-CNPJCPF             - CPF OU CNPJ                 **
      **  SLIGF1-D-FILIAL              - FILIAL                      **
      **  SLIGF1-D-DIGITO              - DIGITO                      **
      **  SLIGF1-D-PESSOA              - TIPO DE PESSOA              **
      **                                 1 = FISICA                  **
      **                                 2 = JURIDICA                **
      **  SLIGF1-T-TPO-REG             - REGISTRO TRAILLER = 9       **
      **  SLIGF1-T-NOM-ARQ             - DESCRICAO DO ARQUIVO        **
      **  SLIGF1-T-QTD-REG             - QUANTIDADE DE DETALHES      **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05 SLIGF1-REGISTRO.
          10 SLIGF1-HEADER.
             15 SLIGF1-H-TPO-REG               PIC 9(001).
             15 SLIGF1-H-NOM-ARQ               PIC X(031).
             15 SLIGF1-H-DATA-GER              PIC X(008).
          10 SLIGF1-DETALHE REDEFINES SLIGF1-HEADER.
             15 SLIGF1-D-TPO-REG               PIC 9(001).
CNPJ7C       15 SLIGF1-D-CNPJCPF               PIC X(009).
CNPJ7C       15 SLIGF1-D-FILIAL                PIC X(004).
             15 SLIGF1-D-DIGITO                PIC 9(002).
             15 SLIGF1-D-PESSOA                PIC 9(001).
             15 FILLER                         PIC X(023).
          10 SLIGF1-TRAILER REDEFINES SLIGF1-HEADER.
             15 SLIGF1-T-TPO-REG               PIC 9(001).
             15 SLIGF1-T-NOM-ARQ               PIC X(031).
             15 SLIGF1-T-QTD-REG               PIC 9(006).
             15 FILLER                         PIC X(002).
