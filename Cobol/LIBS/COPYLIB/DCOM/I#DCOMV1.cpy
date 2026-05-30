      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMV1 *
      * CRIACAO   : 02/2009                                   *
      * DESCRICAO : AREA DE COMUNICACAO                       *
      *                                                       *
      * APLICACAO : MODULO - DCOMXXXX                         *
      *                                                       *
      * TAMANHO   : ENTRADA = LRECL - 6144                    *
      *             SAIDA   = LRECL - 6144                    *
      *********************************************************

       01  WRK-V1-RECEBE-ISD.
           03  WRK-V1-LL-RECEBE     PIC S9(0004) COMP.
           03  WRK-V1-ZZ-RECEBE     PIC S9(0004) COMP.
           03  WRK-V1-TRANSACAO     PIC  X(0009).
           03  WRK-V1-EVENTO        PIC  X(0004).
           03  WRK-V1-FUNCAO        PIC  X(0004).
           03  WRK-V1-ENTRADA       PIC  X(4075).

       01  WRK-V1-SAIDA-MODULO.
           03  WRK-V1-MENSA.
               05  WRK-V1-RETORNO   PIC  9(0004).
               05  WRK-V1-MENSAGEM  PIC  X(0079).
           03  WRK-V1-HEADER.
               05  WRK-V1-RESTART   PIC  9(0005).
               05  WRK-V1-FLAGTERM  PIC  X(0001).
           03  WRK-V1-DADOS.
               05  FILLER           PIC  X(4003).

       01  WRK-V1-TRANSMITE-ISD.
           03  WRK-V1-LL-TRANSMITE  PIC S9(0004) COMP.
           03  WRK-V1-ZZ-TRANSMITE  PIC S9(0004) COMP.
           03  WRK-V1-TRANSMITE     PIC  X(4092).
       01  WRK-V1-TRANSMITE-CICS REDEFINES WRK-V1-TRANSMITE-ISD.
           03 WRK-V1-AREA-TRANSMITE-CICS.
             05  WRK-V1-LL-TRANSMITE-IC30 PIC S9(0004) COMP.
             05  WRK-V1-ZZ-TRANSMITE-IC30 PIC S9(0004) COMP.
             05  WRK-V1-TRANSACAO-IC30 PIC X(0009).
             05  WRK-V1-TRANSMITE-IC30 PIC X(1583).
           03  WRK-V1-FILLER          PIC  X(2500).
