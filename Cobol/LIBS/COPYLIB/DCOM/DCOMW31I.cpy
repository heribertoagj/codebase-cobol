      ******************************************************************
      * NOME BOOK : DCOMW31I - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA LISTA DE FORMULARIOS  IMPRESSAO SAIDA     *
      * DATA      : 04/04/2023                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00296 BYTES                                        *
      ******************************************************************
       05 DCOMW31I-HEADER.
          10 DCOMW31I-COD-LAYOUT             PIC X(08) VALUE 'DCOMW31I'.
          10 DCOMW31I-TAM-LAYOUT             PIC 9(05) VALUE 00296.
       05 DCOMW31I-REGISTRO.
          10 DCOMW31I-ENTRADA.
             15 DCOMW31I-E-DANO-OPER         PIC 9(04).
             15 DCOMW31I-E-NSEQ-OPER         PIC 9(09).
             15 DCOMW31I-E-CPRODT            PIC 9(03).
             15 DCOMW31I-E-CSPROD-DESC       PIC 9(03).
             15 DCOMW31I-E-CONTRATO-LIM      PIC 9(09).
             15 DCOMW31I-E-QTITLO            PIC 9(05).
             15 DCOMW31I-E-CMEIO-ENTRD       PIC 9(03).
             15 DCOMW31I-E-CTPO-DESC         PIC 9(03).
          10 DCOMW31I-BLOCO-SAIDA.
             15 DCOMW31I-INDDOC              PIC X(01).
             15 DCOMW31I-DESCRICAO           PIC X(15).
             15 DCOMW31I-FORMULARIO          PIC X(10).
             15 DCOMW31I-QTDE-VIAS           PIC 9(01).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
