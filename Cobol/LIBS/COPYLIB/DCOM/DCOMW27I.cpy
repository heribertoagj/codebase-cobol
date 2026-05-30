      ******************************************************************
      * NOME BOOK : DCOMW27I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DATA      : 20/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 08379 BYTES                                        *
      ******************************************************************
       05 DCOMW27I-HEADER.
         10 DCOMW27I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW27I'.
         10 DCOMW27I-TAM-LAYOUT        PIC 9(05)   VALUE 08388.
       05 DCOMW27I-REGISTRO.
         10 DCOMW27I-BLOCO-ENTRADA.
           15 DCOMW27I-E-COD-DOCTO                 PIC X(10).
           15 DCOMW27I-E-NUM-OPER                  PIC 9(13).
           15 DCOMW27I-E-USUARIO                   PIC X(09).
           15 DCOMW27I-E-TIPO-VIA                  PIC 9(1).
         10 DCOMW27I-BLOCO-SAIDA.
           15 DCOMW27I-S-CHAVE                     PIC X(40).
           15 DCOMW27I-S-IMPD                      PIC X(8303).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
