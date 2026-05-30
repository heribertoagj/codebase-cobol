      ******************************************************************
      * NOME BOOK : DCOMW24L - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA OPERACAO E GARANTIA                       *
      * DATA      : 03/10/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00206 BYTES                                        *
      ******************************************************************
       05 DCOMW24L-HEADER.
         10 DCOMW24L-COD-LAYOUT              PIC X(08) VALUE 'DCOMW24L'.
         10 DCOMW24L-TAM-LAYOUT                  PIC 9(05) VALUE 00206.
       05 DCOMW24L-REGISTRO.
         10 DCOMW24L-BLOCO-ENTRADA.
           15 DCOMW24L-E-DANO-OPER               PIC 9(04).
           15 DCOMW24L-E-NSEQ-OPER               PIC 9(09).
           15 DCOMW24L-E-CSIT-DESC               PIC 9(03).
           15 DCOMW24L-E-CMEIO-ENTRD-DESC        PIC 9(03).
           15 DCOMW24L-E-CPRODT                  PIC 9(03).
           15 DCOMW24L-E-CSPROD-DESC-COML        PIC 9(03).
           15 DCOMW24L-E-HULT-ATULZ              PIC X(26).
           15 DCOMW24L-E-CTPO-TX-JURO-DESC       PIC 9(03).
         10 DCOMW24L-BLOCO-SAIDA.
           15 DCOMW24L-S-IRSUMO-MEIO-ENTRD       PIC X(15).
           15 DCOMW24L-S-IRSUMO-SIT-DESC         PIC X(15).
           15 DCOMW24L-S-CCART-NORML-DESC        PIC X(05).
           15 DCOMW24L-S-ITPO-TX-JURO-DESC       PIC X(30).
           15 DCOMW24L-S-IRSUMO-TPO-TX-JURO      PIC X(15).
           15 DCOMW24L-S-CSGL-TPO-TX-JURO        PIC X(06).
      *---------------------------------------------------------------*
      *    F I M     D E    B O O K                                   *
      *---------------------------------------------------------------*
