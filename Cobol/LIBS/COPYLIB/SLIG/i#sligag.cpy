      *** AREA DE COMUNICACAO - IMS
      ***
      ***===========================================================***
      *** NOME INC                                     LENGTH=0467  ***
BRQ250*** I#SLIGAG                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICAO DOS PROGRAMAS                ***
      ***            UI - CLIENTE/FORNECEDOR                        ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *                                                                        %
       01  SLIGAG-AREA.
           03 SLIGAG-LL                    PIC S9(004) COMP VALUE +467.
           03 SLIGAG-ZZ                    PIC S9(004) COMP VALUE +0.
           03 SLIGAG-TRANCODE              PIC  X(015).
           03 SLIGAG-SENHAS                PIC  X(037).
           03 SLIGAG-COMANDO               PIC  X(008).
           03 SLIGAG-DADOS-ENVIADOS.
              05 SLIGAG-PERFIL-USUARIO     PIC  X(002).
              05 SLIGAG-CBCO               PIC  9(003).
              05 SLIGAG-CDEPDC             PIC  9(005).
              05 SLIGAG-CFUNC-BDSCO        PIC  9(009).
           03 SLIGAG-DADOS-RETORNADOS.
              05 SLIGAG-COD-RETORNO        PIC  9(002).
              05 SLIGAG-MENSAGEM           PIC  X(079).
           03 SLIGAG-DADOS-PESQUISA.
BRQ250        05 SLIGAG-CEMPR-INC          PIC  9(003).
BRQ250        05 SLIGAG-CDEPDC             PIC  9(005).
BRQ250        05 SLIGAG-CBCO-PRODT-COBR    PIC  9(003).
BRQ250        05 SLIGAG-CIDTFD-PRODT-COBR  PIC  9(002).
BRQ250        05 SLIGAG-CNEGOC-COBR        PIC  9(018).
BRQ250        05 SLIGAG-CIDTFD-CART-LIQDZ  PIC  9(002).
CNPJ7C        05 SLIGAG-CCGC-CPF-CLI       PIC  X(009).
              05 SLIGAG-CINDCD-PSSOA-CLI   PIC  X(001).
              05 SLIGAG-ICOPLT-CLI         PIC  X(040).
              05 SLIGAG-DT-PER-INIC        PIC  X(010).
              05 SLIGAG-DT-PER-FIM         PIC  X(010).
CNPJ7C        05 SLIGAG-CCGC-CPF-SACDO     PIC  X(009).
CNPJ7C        05 SLIGAG-CCGC-CPF-SELECIO   PIC  X(009).
CNPJ7C        05 SLIGAG-CCGC-CPF-PRIM      PIC  X(009).
              05 SLIGAG-CINDCD-PSSOA-SAC   PIC  X(001).
              05 SLIGAG-CBCO-SACDO         PIC  9(003).
              05 SLIGAG-CAG-BCRIA-SACDO    PIC  9(005).
              05 SLIGAG-CCTA-BCRIA-SACDO   PIC  9(013).
CNPJ7C        05 SLIGAG-CCGC-CPF-TELA      PIC  X(009).
              05 SLIGAG-PAG-TELA           PIC  9(008).
AMS054     03 SLIGAG-TRAN100               PIC  X(008).
AMS054     03 SLIGAG-CPOS100               PIC  X(126).
