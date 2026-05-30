      ******************************************************************
      * NOME BOOK : DCOMW24D - BOOK DE INTERFACE COM SERVICO DCOM424D  *
      * DESCRICAO : CONSULTA INFORMACOES DO TIPO DE COBRANCA           *
      * DATA      : 03/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 104 BYTES                                          *
      ******************************************************************
       05 DCOMW24D-HEADER.
         10 DCOMW24D-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW24D'.
         10 DCOMW24D-TAM-LAYOUT        PIC 9(05)   VALUE 104.
       05 DCOMW24D-REGISTRO.
         10 DCOMW24D-BLOCO-ENTRADA.
           15 DCOMW24D-E-CTPO-COBR-TAC             PIC 9(03).
         10 DCOMW24D-BLOCO-SAIDA.
           15 DCOMW24D-S-CTPO-COBR-TAC             PIC 9(03).
           15 DCOMW24D-S-ITPO-COBR-TAC             PIC X(30).
           15 DCOMW24D-S-IRSUMO-COBR-TAC           PIC X(15).
           15 DCOMW24D-S-CSGL-TPO-COBR-TAC         PIC X(06).
           15 DCOMW24D-S-HULT-ATULZ                PIC X(26).
           15 DCOMW24D-S-CTRANS-PROG-ATULZ         PIC X(08).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
