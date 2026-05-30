      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMY0 *
      * CRIACAO   : 08/2006                                   *
      * DESCRICAO : OBTER RATING DO CLIENTE                   *
      *                                                       *
      *                                                       *
      * APLICACAO : MODULO DCOM6414                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01  WRK-AREA-6414.
           03 WRK-ENVIO-6414.
             05 WRK-CCNPJ-CPF-6414             PIC  9(09).
             05 WRK-CFLIAL-CNPJ-6414           PIC  9(04).

      *********************************************************
      * AREA DE RETORNO                                       *
      *********************************************************
           03 WRK-RETORNO-6414.
             05 WRK-COD-RET-6414               PIC  9(04).
             05 WRK-MENS-RET-6414              PIC  X(79).
             05 WRK-CRTING-6414                PIC  X(03).

