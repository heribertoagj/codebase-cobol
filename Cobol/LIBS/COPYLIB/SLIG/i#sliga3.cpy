      *** AREA DE COMUNICACAO - IMS                                     00010000
      *** U.I. BANCO                                                    00020000
      ***===========================================================*** 00030000
      *** NOME INC                                     LENGTH=0300  *** 00040000
      *** I#SLIGA3                                                  *** 00050000
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
       01  SLIGA3-AREA.                                                 00130000
           03 SLIGA3-LL                    PIC S9(004) COMP VALUE +300. 00140000
           03 SLIGA3-ZZ                    PIC S9(004) COMP VALUE +0.   00150000
           03 SLIGA3-TRANCODE              PIC  X(015).                 00160000
           03 SLIGA3-SENHAS                PIC  X(037).                 00170000
           03 SLIGA3-COMANDO               PIC  X(008).                 00180000
           03 SLIGA3-DADOS-ENVIADOS.                                    00190000
              05 SLIGA3-PERFIL-USUARIO     PIC  X(002).                 00200000
              05 SLIGA3-CBCO               PIC  9(003).                 00210000
              05 SLIGA3-CDEPDC             PIC  9(005).                 00220000
              05 SLIGA3-CFUNC-BDSCO        PIC  9(009).                 00230000
           03 SLIGA3-DADOS-RETORNADOS.                                  00240000
              05 SLIGA3-COD-RETORNO        PIC  9(002).                 00250000
              05 SLIGA3-MENSAGEM           PIC  X(079).                 00260000
           03 SLIGA3-DADOS-PESQUISA.                                    00270000
              05 SLIGA3-CEMPR-INC          PIC  9(003).                 00280000
CNPJ7C        05 SLIGA3-CCGC-CPF-SACDO-1   PIC  X(009).                 00290000
              05 SLIGA3-CIDTCD-PSSOA-SACDO-1                            00300000
                                           PIC  X(001).                 00310000
CNPJ7C        05 SLIGA3-CCGC-CPF-SACDO     PIC  X(009).                 00320000
              05 SLIGA3-CIDTCD-PSSOA-SACDO PIC  X(001).                 00330000
CNPJ7C        05 SLIGA3-CCGC-CPF-TELA      PIC  X(009).                 00340000
              05 SLIGA3-PAG-TELA           PIC  9(008).                 00350000
              05 SLIGA3-SEQUENC-PESQ       PIC  9(015).                 00360000
              05 SLIGA3-TPESQ              PIC  X(001).                 00370000
           03 SLIGA3-NOME-EMITENTE         PIC  X(040).                 00380000
BRQB12     03 SLIGA3-DADOS-PESQUISA-NOVO.                               00390000
CNPJ7C        05 SLIGA3-CGCCPF             PIC  X(009).                 00400000
CNPJ7C        05 SLIGA3-CGCFIL             PIC  X(004).                 00410000
'             05 SLIGA3-CGCCTR             PIC  9(002).                 00420000
CNPJ7C        05 SLIGA3-CGCFIL-TELA        PIC  X(004).                 00430000
           03 FILLER                       PIC  X(021).                 00440000
