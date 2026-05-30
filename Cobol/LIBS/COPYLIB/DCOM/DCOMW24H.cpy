      ******************************************************************
      * NOME BOOK : DCOMW24H - BOOK DE INTERFACE COM SERVICO DCOM424H  *
      * DESCRICAO : CONSULTA INFORMACOES DO TDOCTO FORML DESC          *
      * DATA      : 05/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 402 BYTES                                          *
      ******************************************************************
       05 DCOMW24H-HEADER.
         10 DCOMW24H-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW24H'.
         10 DCOMW24H-TAM-LAYOUT        PIC 9(05)   VALUE 00402.
       05 DCOMW24H-REGISTRO.
         10 DCOMW24H-BLOCO-ENTRADA.
           15 DCOMW24H-E-CDOCTO-FORML-DESC         PIC X(10).
         10 DCOMW24H-BLOCO-SAIDA.
           15 DCOMW24H-S-CDOCTO-FORML-DESC         PIC X(10).
           15 DCOMW24H-S-CINDCD-MODLD-DESC         PIC X(01).
           15 DCOMW24H-S-CTPO-DOCTO-FORML          PIC 9(03).
           15 DCOMW24H-S-IDOCTO-FORML-DESC         PIC X(40).
           15 DCOMW24H-S-IRSUMO-DOCTO-FORML        PIC X(15).
           15 DCOMW24H-S-CMOD-DOCTO-FORML          PIC 9(05).
           15 DCOMW24H-S-QVIA-EMIS-DOCTO           PIC 9(01).
           15 DCOMW24H-S-CINDCD-VIA-DIFER          PIC X(01).
           15 DCOMW24H-S-RDOCTO-FORML-DESC         PIC X(240).
           15 DCOMW24H-S-CSIT-DESC-COML            PIC 9(03).
           15 DCOMW24H-S-HSIT-DESC-COML            PIC X(26).
           15 DCOMW24H-S-CTRANS-PROG-ATULZ         PIC X(08).
           15 DCOMW24H-S-HULT-ATULZ                PIC X(26).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
