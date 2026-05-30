      ******************************************************************
      * NOME BOOK : DCOMW26I - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA LISTA DE FORMULARIOS  IMPRESSAO SAIDA     *
      * DATA      : 20/10/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00296 BYTES                                        *
      ******************************************************************
       05 DCOMW26I-HEADER.
          10 DCOMW26I-COD-LAYOUT             PIC X(08) VALUE 'DCOMW26I'.
          10 DCOMW26I-TAM-LAYOUT             PIC 9(05) VALUE 00296.
       05 DCOMW26I-REGISTRO.
          10 DCOMW26I-ENTRADA.
             15 DCOMW26I-E-DANO-OPER         PIC 9(04).
             15 DCOMW26I-E-NSEQ-OPER         PIC 9(09).
             15 DCOMW26I-E-CPRODT            PIC 9(03).
             15 DCOMW26I-E-CSPROD-DESC       PIC 9(03).
             15 DCOMW26I-E-CONTRATO-LIM      PIC 9(09).
             15 DCOMW26I-E-QTITLO            PIC 9(05).
             15 DCOMW26I-E-CMEIO-ENTRD       PIC 9(03).
             15 DCOMW26I-E-CTPO-DESC         PIC 9(03).
          10 DCOMW26I-BLOCO-SAIDA.
             15 DCOMW26I-S1-QTD-FORM         PIC 9(01).
             15 DCOMW26I-S1-FORMS            OCCURS  9 TIMES.
                20 DCOMW26I-INDDOC              PIC X(01).
                20 DCOMW26I-DESCRICAO           PIC X(15).
                20 DCOMW26I-FORMULARIO          PIC X(10).
                20 DCOMW26I-QTDE-VIAS           PIC 9(01).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
