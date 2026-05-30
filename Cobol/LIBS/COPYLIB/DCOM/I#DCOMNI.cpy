      *****************************************************************
      * SISTEMA...: DCOM - DESCONTO COMERCIAL        NOME: I#DCOMNI   *
      * DESCRICAO.: INTERFACE DCOM X TRIC                             *
      * TAMANHO...: LRECL = 200                                       *
      *****************************************************************
DTS001* ALTERACOES:                                                   *
DTS001*    - 17/12/2011 - INCLUIR DATA INICIO DA OPERACAO, TIPO DE    *
DTS001*                   DESCONTO E TIPO DE COMERCIALIZACAO.         *
DTS001*===============================================================*
      *
       01  DCOMNI-TRIC-REG.
           05  DCOMNI-BCO              PIC  9(003).
           05  DCOMNI-AGC              PIC  9(005).
           05  DCOMNI-CTA              PIC  9(007).
           05  DCOMNI-DTA              PIC  X(010).
           05  DCOMNI-PRODT            PIC  9(003).
           05  DCOMNI-SPROD            PIC  9(003).
           05  DCOMNI-FATGER           PIC  9(003).
           05  DCOMNI-TPPESS           PIC  X(001).
           05  DCOMNI-MEENT            PIC  9(003).
           05  DCOMNI-SITDES           PIC  9(003).
           05  DCOMNI-VALOR-1          PIC  9(013)V99 COMP-3.
           05  DCOMNI-VALOR-2          PIC  9(013)V99 COMP-3.
           05  DCOMNI-VALOR-3          PIC  9(013)V99 COMP-3.
           05  DCOMNI-VALOR-4          PIC  9(013)V99 COMP-3.
           05  DCOMNI-VALOR-5          PIC  9(013)V99 COMP-3.
           05  DCOMNI-CONTCOR          PIC  9(007).
           05  DCOMNI-STR              PIC  X(003).
           05  DCOMNI-BCO-DST          PIC  9(003).
           05  DCOMNI-AGC-DST          PIC  9(005).
           05  DCOMNI-CTA-DST          PIC  9(007).
           05  DCOMNI-HIST-COML        PIC  X(032).
           05  DCOMNI-GRP-CTBIL        PIC  9(003).
           05  DCOMNI-SGRP-CTBIL       PIC  9(003).
DTS001     05  DCOMNI-DINIC-OPER-DESC  PIC  X(010).
DTS001     05  DCOMNI-CTPO-DESC-COML   PIC  9(003)    COMP-3.
DTS001     05  DCOMNI-TPO-COMCZ-RURAL  PIC  X(001).
DTS001     05  FILLER                  PIC  X(043).
