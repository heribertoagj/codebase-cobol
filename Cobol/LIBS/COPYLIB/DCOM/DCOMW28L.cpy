      ******************************************************************
      * NOME BOOK : DCOMW28L - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA TITULOS DA OPERACAO                       *
      * DATA      : 13/11/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 07281 BYTES                                        *
      ******************************************************************
       05 DCOMW28L-HEADER.
         10 DCOMW28L-COD-LAYOUT              PIC X(08) VALUE 'DCOMW28L'.
         10 DCOMW28L-TAM-LAYOUT                  PIC 9(05) VALUE 07281.
       05 DCOMW28L-REGISTRO.
         10 DCOMW28L-BLOCO-ENTRADA.
           15 DCOMW28L-E-DANO-OPER               PIC 9(04).
           15 DCOMW28L-E-NSEQ-OPER               PIC 9(09).
           15 DCOMW28L-E-CTPO-DESC               PIC 9(03).
         10 DCOMW28L-BLOCO-SAIDA.
           15 DCOMW28L-S-QTD-TITULOS             PIC 9(02).
           15 DCOMW28L-S-SAI-OCORRENCIAS.
             20  DCOMW28L-S-OCORRENCIAS OCCURS 50 TIMES.
               25 DCOMW28L-S-CTITLO-COBR-BCO     PIC X(20).
               25 DCOMW28L-S-NOME-SACADO         PIC X(40).
               25 DCOMW28L-S-CNPJ                PIC 9(09).
               25 DCOMW28L-S-FILIAL              PIC 9(04).
               25 DCOMW28L-S-CTRL                PIC 9(02).
               25 DCOMW28L-S-DVCTO-TITLO         PIC X(10).
               25 DCOMW28L-S-VALOR-IOF      PIC 9(09)V9(06) VALUE ZEROS.
               25 DCOMW28L-S-VALOR-JUROS    PIC 9(09)V9(06) VALUE ZEROS.
               25 DCOMW28L-S-VTITLO-DESC-COML    PIC 9(13)V99.
               25 DCOMW28L-S-VALOR-LIQUIDO       PIC 9(13)V99.
      *---------------------------------------------------------------*
      *    F I M     D E    B O O K                                   *
      *---------------------------------------------------------------*
