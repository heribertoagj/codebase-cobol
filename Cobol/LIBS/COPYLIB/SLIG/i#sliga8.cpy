      *** AREA DE COMUNICACAO - IMS                                     00010000
      ***                                                               00020000
      ***===========================================================*** 00030000
      *** NOME INC                                     LENGTH=0340  *** 00040000
      *** I#SLIGA8             GRUPO ECONOMICO                      *** 00050000
      ***-----------------------------------------------------------*** 00060000
      ***                                                           *** 00070000
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          *** 00080000
      ***            AREA DE COMUNICAO DOS PROGRAMAS                *** 00090000
      ***            COM OS PROGRAMAS CHAMADOS                      *** 00100000
BRQ250***            INCLUSÃO DOS CAMPOS FILIAL E CONTROLE          *** 00100100
      ***===========================================================*** 00110000
      *DATA        PROJETO                                 ANALISTA    *00110100
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*00110200
      *----------------------------------------------------------------*00110300
      *                                                                 00120000
       01  SLIGA8-AREA.                                                 00130000
           03 SLIGA8-LL                    PIC S9(004) COMP VALUE +340. 00140000
           03 SLIGA8-ZZ                    PIC S9(004) COMP VALUE +0.   00150000
           03 SLIGA8-TRANCODE              PIC  X(015).                 00160000
           03 SLIGA8-SENHAS                PIC  X(037).                 00170000
           03 SLIGA8-COMANDO               PIC  X(008).                 00180000
           03 SLIGA8-DADOS-ENVIADOS.                                    00190000
              05 SLIGA8-PERFIL-USUARIO     PIC  X(002).                 00200000
              05 SLIGA8-CBCO               PIC  9(003).                 00210000
              05 SLIGA8-CDEPDC             PIC  9(005).                 00220000
              05 SLIGA8-CFUNC-BDSCO        PIC  9(009).                 00230000
           03 SLIGA8-DADOS-RETORNADOS.                                  00240000
              05 SLIGA8-COD-RETORNO        PIC  9(002).                 00250000
              05 SLIGA8-MENSAGEM           PIC  X(079).                 00260000
           03 SLIGA8-DADOS-INFORMADOS.                                  00270000
              05 SLIGA8-GRUPO-DIGITADO     PIC  9(009).                 00280000
              05 SLIGA8-PERIODO-DE         PIC  X(010).                 00290000
              05 SLIGA8-PERIODO-ATE        PIC  X(010).                 00300000
           03 SLIGA8-DADOS-OBTIDOS.                                     00310000
              05 SLIGA8-NOME-GRUPO         PIC  X(030).                 00320000
              05 SLIGA8-PERIODO-ANT        PIC  X(008).                 00330000
              05 SLIGA8-PERIODO-ANT-1      PIC  X(008).                 00340000
              05 SLIGA8-PERIODO-ANT-2      PIC  X(008).                 00350000
CNPJ7C        05 SLIGA8-CCGC-CPF-SACDO     PIC  X(009).                 00360000
              05 SLIGA8-CIDTCD-PSSOA-SACDO PIC  X(001).                 00370000
              05 SLIGA8-ICOPLT-SACDO       PIC  X(040).                 00380000
CNPJ7C        05 SLIGA8-CCGC-CPF-TELA      PIC  X(009).                 00390000
              05 SLIGA8-PAG-TELA           PIC  9(008).                 00400000
CNPJ7C        05 SLIGA8-FILIAL-SACDO       PIC  X(005).                 00400100
BRQ250        05 SLIGA8-CONTR-SACDO        PIC  9(002).                 00400200
BRQ250        05 SLIGA8-TP-FORMATO         PIC  X(001).                 00400300
BRQ250     03 SLIGA8-DADOS-SLIG2860.                                    00480000
BRQ250        05 SLIGA8-PARTICIP           PIC  X(006) OCCURS 3.        00481000
