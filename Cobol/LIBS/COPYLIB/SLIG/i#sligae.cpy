      *** AREA DE COMUNICACAO - IMS                                     00010001
      ***                                                               00020001
      ***===========================================================*** 00030001
      *** NOME INC                                     LENGTH=0350  *** 00040001
      *** I#SLIGAE                                                  *** 00050001
      ***-----------------------------------------------------------*** 00060001
      ***                                                           *** 00070001
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          *** 00080001
      ***            AREA DE COMUNICAO DOS PROGRAMAS                *** 00090001
      ***            UI - DEPENDENCIA                               *** 00100001
      ***===========================================================*** 00110001
      *DATA        PROJETO                                 ANALISTA    *00110002
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*00110003
      *----------------------------------------------------------------*00110004
      *                                                                 00120001
       01  SLIGAE-AREA.                                                 00130001
           03 SLIGAE-LL                    PIC S9(004) COMP VALUE +350. 00140001
           03 SLIGAE-ZZ                    PIC S9(004) COMP VALUE +0.   00150001
           03 SLIGAE-TRANCODE              PIC  X(015).                 00160001
           03 SLIGAE-SENHAS                PIC  X(037).                 00170001
           03 SLIGAE-COMANDO               PIC  X(008).                 00180001
           03 SLIGAE-DADOS-ENVIADOS.                                    00190001
              05 SLIGAE-PERFIL-USUARIO     PIC  X(002).                 00200001
              05 SLIGAE-CBCO               PIC  9(003).                 00210001
              05 SLIGAE-CDEPDC             PIC  9(005).                 00220001
              05 SLIGAE-CFUNC-BDSCO        PIC  9(009).                 00230001
           03 SLIGAE-DADOS-RETORNADOS.                                  00240001
              05 SLIGAE-COD-RETORNO        PIC  9(002).                 00250001
              05 SLIGAE-MENSAGEM           PIC  X(079).                 00260001
           03 SLIGAE-DADOS-PESQUISA.                                    00270001
              05 SLIGAE-CBCOP              PIC  9(003).                 00280001
              05 SLIGAE-CDEPDCP            PIC  9(005).                 00290001
CNPJ7C        05 SLIGAE-CCGC-CPF           PIC  X(009).                 00300001
CNPJ7C        05 SLIGAE-CCGC-CPF-FILIAL    PIC  X(005).                 00320002
              05 SLIGAE-CCGC-CPF-CONTR     PIC  9(002).                 00320004
              05 SLIGAE-CINDCD-PSSOA       PIC  X(001).                 00320007
              05 SLIGAE-TP-FORMATO         PIC  X(001).                 00320008
              05 SLIGAE-CBCO-SACDO         PIC  9(003).                 00330001
              05 SLIGAE-CAG-BCRIA-SACDO    PIC  9(005).                 00340001
              05 SLIGAE-CCTA-BCRIA-SACDO   PIC  9(013).                 00350001
CNPJ7C        05 SLIGAE-CCGC-CPF-TELA      PIC  X(009).                 00360001
CNPJ7C        05 SLIGAE-CFLIAL-CPF-TELA    PIC  X(004).                 00370001
              05 SLIGAE-CCTRL-CPF-TELA     PIC  9(002).                 00380001
              05 SLIGAE-PAG-TELA           PIC  9(008).                 00390001
      * FORMATO DATE - DD.MM.AAAA                                       00400001
              05 SLIGAE-PERIODO-INI        PIC  X(010).                 00410001
      * FORMATO DATE - DD.MM.AAAA                                       00420001
              05 SLIGAE-PERIODO-FIM        PIC  X(010).                 00430001
              05 SLIGAE-PAG-TELA-2         PIC  9(003).                 00431001
              05 SLIGAE-SEQ-BANDA          PIC  9(015).                 00432001
           03 SLIGAE-NOME-EMITENTE         PIC  X(040).                 00433001
           03 FILLER                       PIC  X(038).                 00434001
