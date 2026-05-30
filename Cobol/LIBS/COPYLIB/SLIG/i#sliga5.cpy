      *** AREA DE COMUNICACAO - IMS                                     00010000
      *** U.I. SACADO / EMITENTE / COMPRADOR                            00020000
      ***===========================================================*** 00030000
      *** NOME INC                                     LENGTH=0300  *** 00040000
      *** I#SLIGA5                                                  *** 00050000
      ***-----------------------------------------------------------*** 00060000
      ***                                                           *** 00070000
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          *** 00080000
      ***            AREA DE COMUNICAO DE PROGRAMA ON-LINE          *** 00090000
      ***            COM OS PROGRAMAS CHAMADOS                      *** 00100000
      ***===========================================================*** 00110000
      *DATA        PROJETO                                 ANALISTA    *00110100
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*00110200
      *----------------------------------------------------------------*00110300
      *                                                                 00120000
       01  SLIGA5-AREA.                                                 00130000
           03 SLIGA5-LL                    PIC S9(004) COMP VALUE +300. 00140000
           03 SLIGA5-ZZ                    PIC S9(004) COMP VALUE +0.   00150000
           03 SLIGA5-TRANCODE              PIC  X(015).                 00160000
           03 SLIGA5-SENHAS                PIC  X(037).                 00170000
           03 SLIGA5-COMANDO               PIC  X(008).                 00180000
           03 SLIGA5-DADOS-ENVIADOS.                                    00190000
              05 SLIGA5-PERFIL-USUARIO     PIC  X(002).                 00200000
              05 SLIGA5-CBCO               PIC  9(003).                 00210000
              05 SLIGA5-CDEPDC             PIC  9(005).                 00220000
              05 SLIGA5-CFUNC-BDSCO        PIC  9(009).                 00230000
           03 SLIGA5-DADOS-RETORNADOS.                                  00240000
              05 SLIGA5-COD-RETORNO        PIC  9(002).                 00250000
              05 SLIGA5-MENSAGEM           PIC  X(079).                 00260000
           03 SLIGA5-DADOS-PESQUISA.                                    00270000
              05 SLIGA5-SACDO-EMITT-COMPR.                              00280000
CNPJ7C          10 SLIGA5-CCGC-CPF         PIC  X(009).                 00290000
                10 SLIGA5-CINDCD-PSSOA     PIC  X(001).                 00300000
           03 SLIGA5-DADOS-OBTIDOS.                                     00310000
              05 SLIGA5-PERIODO-DE         PIC  X(010).                 00320000
              05 SLIGA5-PERIODO-ATE        PIC  X(010).                 00330000
              05 SLIGA5-NOME-SACADO        PIC  X(040).                 00340000
BRQA12     03 SLIGA5-DADOS-PESQUISA-NOVO.                               00350000
'             05 SLIGA5-NOVO-SACDO-EMITT-COMPR.                         00360000
CNPJ7C          10 SLIGA5-CNPJ-CPF         PIC  X(009).                 00370000
CNPJ7C          10 SLIGA5-CNPJ-FIL         PIC  X(004).                 00380000
'               10 SLIGA5-CNPJ-CTR         PIC  9(002).                 00390000
BRQA12        05 SLIGA5-TP-FORMATO         PIC  X(001).                 00390100
           03 FILLER                       PIC  X(050).                 00400000
