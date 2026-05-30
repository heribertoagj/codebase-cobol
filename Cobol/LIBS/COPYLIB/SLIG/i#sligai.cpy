      *** AREA DE COMUNICACAO - IMS
      *** U.I. BANCO
      ***===========================================================***
      *** NOME INC                                     LENGTH=0300  ***
      *** I#SLIGAI                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICAO DE PROGRAMA ON-LINE          ***
      ***            COM OS PROGRAMAS CHAMADOS                      ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *
       01  SLIGAI-AREA.
           03 SLIGAI-LL                    PIC S9(004) COMP VALUE +300.
           03 SLIGAI-ZZ                    PIC S9(004) COMP VALUE +0.
           03 SLIGAI-TRANCODE              PIC  X(015).
           03 SLIGAI-SENHAS                PIC  X(037).
           03 SLIGAI-COMANDO               PIC  X(008).
           03 SLIGAI-DADOS-ENVIADOS.
              05 SLIGAI-PERFIL-USUARIO     PIC  X(002).
              05 SLIGAI-CBCO               PIC  9(003).
              05 SLIGAI-CDEPDC             PIC  9(005).
              05 SLIGAI-CFUNC-BDSCO        PIC  9(009).
           03 SLIGAI-DADOS-RETORNADOS.
              05 SLIGAI-COD-RETORNO        PIC  9(002).
              05 SLIGAI-MENSAGEM           PIC  X(079).
           03 SLIGAI-DADOS-PESQUISA.
              05 SLIGAI-CEMPR-INC          PIC  9(003).
CNPJ7C        05 SLIGAI-CCGC-CPF-SACDO-1   PIC  X(009).
CNPJ7C        05 SLIGA4-CCGC-CPF-FILIAL-1  PIC  X(005).
              05 SLIGA4-CCGC-CPF-CONTR-1   PIC  9(002).
              05 SLIGA4-TP-FORMATO-1       PIC  X(001).
              05 SLIGAI-CIDTCD-PSSOA-SACDO-1
                                           PIC  X(001).
CNPJ7C        05 SLIGAI-CCGC-CPF-SACDO     PIC  X(009).
CNPJ7C        05 SLIGA4-CCGC-CPF-FILIAL    PIC  X(005).
              05 SLIGA4-CCGC-CPF-CONTR     PIC  9(002).
              05 SLIGA4-TP-FORMATO         PIC  X(001).
              05 SLIGAI-CIDTCD-PSSOA-SACDO PIC  X(001).
CNPJ7C        05 SLIGAI-CCGC-CPF-TELA      PIC  X(009).
CNPJ7C        05 SLIGA4-CCGC-CPF-FILIAL-T  PIC  X(005).
              05 SLIGA4-CCGC-CPF-CONTR-T   PIC  9(002).
              05 SLIGA4-TP-FORMATO-T       PIC  X(001).
              05 SLIGAI-PAG-TELA           PIC  9(008).
              05 SLIGAI-SEQUENC-PESQ       PIC  9(015).
              05 SLIGAI-TPESQ              PIC  X(001).
           03 SLIGAI-NOME-EMITENTE         PIC  X(040).
           03 FILLER                       PIC  X(016).
