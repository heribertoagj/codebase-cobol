      *****************************************************************
      * SISTEMA...: DCOM - DESCONTO COMERCIAL        NOME: I#DCOMS2   *
      * DESCRICAO.: SALDOS DAS OPERACOES (AVISOS E EXTRATOS)          *
      * TAMANHO...: LRECL = 156                                       *
      *****************************************************************
       01  S2-SALDO-REGISTRO.
  1  1     03  S2-CTPO-CLIENTE             PIC S9(01)V        COMP-3.
  2  5     03  S2-CNPJ-CPF-DESTINO         PIC S9(09)V        COMP-3.
  7  3     03  S2-CFLIAL-CNPJ-DESTINO      PIC S9(05)V        COMP-3.
 10  2     03  S2-CCTRL-CNPJ-DESTINO       PIC S9(02)V        COMP-3.
 12  2     03  S2-CSPROD-DESC              PIC S9(03)V        COMP-3.
 14 10     03  S2-DMOVTC-DESC-COML         PIC  X(10).
 24  3     03  S2-DANO-OPER-DESC           PIC S9(04)V        COMP-3.
 27  5     03  S2-NSEQ-OPER-DESC           PIC S9(09)V        COMP-3.
 32  9     03  S2-VPRINC-AVENCER           PIC S9(15)V99      COMP-3.
 41  9     03  S2-VSDO-DVDOR-OPER          PIC S9(15)V99      COMP-3.
 50  9     03  S2-DVCTO-PTRINTA            PIC S9(15)V99      COMP-3.
 59  9     03  S2-DVCTO-PSESSENTA          PIC S9(15)V99      COMP-3.
 68  9     03  S2-DVCTO-ASESSENTA          PIC S9(15)V99      COMP-3.
      *--->>   DADOS COMPLEMENTARES DO SUB PRODUTO   ---
 77 10     03  S2-DINIC-PER-AV             PIC  X(10).
 87 10     03  S2-DFNAL-PER-AV             PIC  X(10).
 97 10     03  S2-NM-REDUZIDO              PIC  X(10).
107  9     03  S2-VPRINC-AVENCER-DIA       PIC S9(15)V99      COMP-3.
116  9     03  S2-VRAP-PSENT-OPER          PIC S9(15)V99      COMP-3.
125  2     03  S2-CTPO-DESC-COML           PIC S9(03)V        COMP-3.
127  3     03  S2-CJUNC-DEPDC-DEST         PIC S9(05)V        COMP-3.
130  7     03  S2-CCCORR-DEST              PIC S9(13)V        COMP-3.
137  3     03  S2-CGRP-CTBIL-DEST          PIC S9(05)V        COMP-3.
140  2     03  S2-CSGRP-CTBIL-DEST         PIC S9(03)V        COMP-3.
142 15     03  S2-FILLER                   PIC  X(15).
