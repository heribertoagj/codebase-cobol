      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMUN *
      * CRIACAO   : 10/2005                                   *
      * DESCRICAO : AREA DE COMUNICACAO                       *
      *                                                       *
      * APLICACAO : MODULO - DCOM0001                         *
      *                                                       *
      * TAMANHO   : ENTRADA = LRECL - 6144                    *
      *             SAIDA   = LRECL - 6144                    *
      *********************************************************
       01  UN-RECEBE-ISD.
           03  UN-LL-RECEBE         PIC S9(0004) COMP.
           03  UN-ZZ-RECEBE         PIC S9(0004) COMP.
           03  UN-TRANSACAO         PIC  X(0009).
           03  UN-EVENTO            PIC  X(0004).
           03  UN-FUNCAO            PIC  X(0004).
           03  UN-ENTRADA           PIC  X(4075).

       01  UN-ENTRADA-MODULO        PIC  X(4075).

       01  UN-SAIDA-MODULO.
           03  UN-MENSA.
               05  UN-RETORNO       PIC  9(0004).
               05  UN-MENSAGEM      PIC  X(0079).
           03  UN-HEADER.
               05  UN-RESTART       PIC  9(0005).
               05  UN-FLAGTERM      PIC  X(0001).
           03  UN-DADOS.
               05  FILLER           PIC  X(4003).

       01  UN-TRANSMITE-ISD.
           03  UN-LL-TRANSMITE      PIC S9(0004) COMP.
           03  UN-ZZ-TRANSMITE      PIC S9(0004) COMP.
           03  UN-TRANSMITE         PIC  X(4092).
       01  UN-TRANSMITE-CICS REDEFINES UN-TRANSMITE-ISD.
           03 UN-AREA-TRANSMITE-CICS.
             05  UN-LL-TRANSMITE-IC30 PIC S9(0004) COMP.
             05  UN-ZZ-TRANSMITE-IC30 PIC S9(0004) COMP.
             05  UN-TRANSACAO-IC30    PIC  X(0009).
             05  UN-TRANSMITE-IC30    PIC  X(1583).
           03  UN-FILLER              PIC  X(2500).
