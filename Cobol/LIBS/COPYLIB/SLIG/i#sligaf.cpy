      *** AREA DE COMUNICACAO - IMS
      ***
      ***===========================================================***
      *** NOME INC                                     LENGTH=0490  ***
      *** I#SLIGAF                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICAO DOS PROGRAMAS                ***
      ***            UI - CLIENTE/FORNECEDOR                        ***
      ***                                                           ***
      ***            BASE DA I#SLIGA7 - INCLUSÃO DOS CAMPOS         ***
      ***            FILIAL E CONTROLE, TIPO DO FORMATO             ***
      ***            BCO, AGENCIA E CONTA DO CLIENTE                ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *                                                                        %
       01  SLIGAF-AREA.
           03 SLIGAF-LL                    PIC S9(004) COMP VALUE +490.
           03 SLIGAF-ZZ                    PIC S9(004) COMP VALUE +0.
           03 SLIGAF-TRANCODE              PIC  X(015).
           03 SLIGAF-SENHAS                PIC  X(037).
           03 SLIGAF-COMANDO               PIC  X(008).
           03 SLIGAF-DADOS-ENVIADOS.
              05 SLIGAF-PERFIL-USUARIO     PIC  X(002).
              05 SLIGAF-CBCO               PIC  9(003).
              05 SLIGAF-CDEPDC             PIC  9(005).
              05 SLIGAF-CFUNC-BDSCO        PIC  9(009).
           03 SLIGAF-DADOS-RETORNADOS.
              05 SLIGAF-COD-RETORNO        PIC  9(002).
              05 SLIGAF-MENSAGEM           PIC  X(079).
           03 SLIGAF-DADOS-PESQUISA.
CNPJ7C        05 SLIGAF-CCGC-CPF-CLI       PIC  X(009).
CNPJ7C        05 SLIGAF-CCGC-CPF-FILIAL-C  PIC  X(005).
              05 SLIGAF-CCGC-CPF-CONTR-C   PIC  9(002).
              05 SLIGAF-TP-FORMATO-C       PIC  X(001).
              05 SLIGAF-CINDCD-PSSOA-CLI   PIC  X(001).
              05 SLIGAF-ICOPLT-CLI         PIC  X(040).
              05 SLIGAF-DT-PER-INIC        PIC  X(010).
              05 SLIGAF-DT-PER-FIM         PIC  X(010).
CNPJ7C        05 SLIGAF-CCGC-CPF-SACDO     PIC  X(009).
CNPJ7C        05 SLIGAF-CCGC-CPF-FILIAL-SAC PIC X(005).
              05 SLIGAF-CCGC-CPF-CONTR-SAC PIC  9(002).
              05 SLIGAF-TP-FORMATO-SAC     PIC  X(001).
CNPJ7C        05 SLIGAF-CCGC-CPF-SELECIO   PIC  X(009).
CNPJ7C        05 SLIGAF-CCGC-CPF-PRIM      PIC  X(009).
              05 SLIGAF-CINDCD-PSSOA-SAC   PIC  X(001).
              05 SLIGAF-ICOPLT-SAC         PIC  X(040).
              05 SLIGAF-CBCO-SAC-CLI       PIC  9(003).
              05 SLIGAF-CAG-BCRIA-SAC-CLI  PIC  9(005).
              05 SLIGAF-CCTA-BCRIA-SAC-CLI PIC  9(013).
              05 SLIGAF-CBCO-SAC           PIC  9(003).
              05 SLIGAF-CAG-BCRIA-SAC      PIC  9(005).
              05 SLIGAF-CCTA-BCRIA-SAC     PIC  9(013).
CNPJ7C        05 SLIGAF-CCGC-CPF-TELA      PIC  X(009).
              05 SLIGAF-PAG-TELA           PIC  9(008).
AMS054     03 SLIGAF-TRAN100               PIC  X(008).
AMS054     03 SLIGAF-CPOS100               PIC  X(126).
