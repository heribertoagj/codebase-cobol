      *** AREA DE COMUNICACAO - IMS
      ***
      ***===========================================================***
      *** NOME INC                                     LENGTH=0300  ***
      *** I#SLIGA7                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICAO DOS PROGRAMAS                ***
      ***            UI - CLIENTE/FORNECEDOR                        ***
      ***===========================================================***
      *                                                                        %
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       01  SLIGA7-AREA.
           03 SLIGA7-LL                    PIC S9(004) COMP VALUE +434.
           03 SLIGA7-ZZ                    PIC S9(004) COMP VALUE +0.
           03 SLIGA7-TRANCODE              PIC  X(015).
           03 SLIGA7-SENHAS                PIC  X(037).
           03 SLIGA7-COMANDO               PIC  X(008).
           03 SLIGA7-DADOS-ENVIADOS.
              05 SLIGA7-PERFIL-USUARIO     PIC  X(002).
              05 SLIGA7-CBCO               PIC  9(003).
              05 SLIGA7-CDEPDC             PIC  9(005).
              05 SLIGA7-CFUNC-BDSCO        PIC  9(009).
           03 SLIGA7-DADOS-RETORNADOS.
              05 SLIGA7-COD-RETORNO        PIC  9(002).
              05 SLIGA7-MENSAGEM           PIC  X(079).
           03 SLIGA7-DADOS-PESQUISA.
CNPJ7C        05 SLIGA7-CCGC-CPF-CLI       PIC  X(009).
              05 SLIGA7-CINDCD-PSSOA-CLI   PIC  X(001).
              05 SLIGA7-ICOPLT-CLI         PIC  X(040).
              05 SLIGA7-DT-PER-INIC        PIC  X(010).
              05 SLIGA7-DT-PER-FIM         PIC  X(010).
CNPJ7C        05 SLIGA7-CCGC-CPF-SACDO     PIC  X(009).
CNPJ7C        05 SLIGA7-CCGC-CPF-SELECIO   PIC  X(009).
CNPJ7C        05 SLIGA7-CCGC-CPF-PRIM      PIC  X(009).
              05 SLIGA7-CINDCD-PSSOA-SAC   PIC  X(001).
              05 SLIGA7-CBCO-SACDO         PIC  9(003).
              05 SLIGA7-CAG-BCRIA-SACDO    PIC  9(005).
              05 SLIGA7-CCTA-BCRIA-SACDO   PIC  9(013).
CNPJ7C        05 SLIGA7-CCGC-CPF-TELA      PIC  X(009).
              05 SLIGA7-PAG-TELA           PIC  9(008).
AMS054     03 SLIGA7-TRAN100               PIC  X(008).
AMS054     03 SLIGA7-CPOS100               PIC  X(126).
