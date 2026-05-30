      ******************************************************************
      * NOME BOOK : DCOMW20L - BOOK DE INTERFACE                       *
      * DESCRICAO : LISTA DE PRODUTO   COMBO IMPRESSAO                 *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 07370 BYTES                                        *
      ******************************************************************
       05 DCOMW20L-HEADER.
         10 DCOMW20L-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW20L'.
         10 DCOMW20L-TAM-LAYOUT        PIC 9(05)   VALUE 07370.
       05 DCOMW20L-REGISTRO.
         10 DCOMW20L-BLOCO-ENTRADA.
           15 DCOMW20L-E-QTDE-DESP                 PIC 9(05).
           15 DCOMW20L-E-INSTRUCAO                 PIC X(10).
         10 DCOMW20L-BLOCO-SAIDA.
           15 DCOMW20L-S-RESTART                   PIC 9(05).
           15 DCOMW20L-S-QTDE-RET                  PIC 9(02).
           15 DCOMW20L-S-QTDE-TOT                  PIC 9(09).
           15 DCOMW20L-S-OCORRENCIAS     OCCURS 0 TO 99 TIMES
            DEPENDING ON             DCOMW20L-S-QTDE-RET.
             20 DCOMW20L-S-CPRODT                  PIC  9(03).
             20 DCOMW20L-S-IPRODT                  PIC  X(60).
             20 DCOMW20L-S-IABREV-PRODT            PIC  X(12).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
