      ******************************************************************
      * NOME BOOK : DCOMW24J - BOOK DE INTERFACE COM SERVICO DCOM424J  *
      * DESCRICAO : CONSULTA INFORMACOES DO TTPO DOCTO FORML           *
      * DATA      : 06/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 108 BYTES                                          *
      ******************************************************************
       05 DCOMW24J-HEADER.
         10 DCOMW24J-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW24J'.
         10 DCOMW24J-TAM-LAYOUT        PIC 9(05)   VALUE 00108.
       05 DCOMW24J-REGISTRO.
         10 DCOMW24J-BLOCO-ENTRADA.
           15 DCOMW24J-E-CTPO-DOCTO-FORML          PIC 9(03).
         10 DCOMW24J-BLOCO-SAIDA.
           15 DCOMW24J-S-CTPO-DOCTO-FORML          PIC 9(03).
           15 DCOMW24J-S-ITPO-DOCTO-FORML          PIC X(40).
           15 DCOMW24J-S-IRSUMO-TPO-DOCTO          PIC X(15).
           15 DCOMW24J-S-HULT-ATULZ                PIC X(26).
           15 DCOMW24J-S-CTRANS-PROG-ATULZ         PIC X(08).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
