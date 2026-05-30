      ******************************************************************
      * NOME BOOK : DCOMW31E - BOOK DE ENTRADA                         *
      * DESCRICAO : CONSULTA LISTA DE FORMULARIOS  IMPRESSAO           *
      * DATA      : 04/04/2023                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00052 BYTES                                        *
      ******************************************************************
           05 DCOMW31E-HEADER.
              10 DCOMW31E-COD-LAYOUT      PIC X(08) VALUE 'DCOMW31E'.
              10 DCOMW31E-TAM-LAYOUT      PIC 9(05) VALUE 00052.
           05 DCOMW31E-REGISTRO.
              10 DCOMW31E-AREA-ENTRADA.
                 15 DCOMW31E-E-DANO-OPER            PIC 9(04).
                 15 DCOMW31E-E-NSEQ-OPER            PIC 9(09).
                 15 DCOMW31E-E-CPRODT               PIC 9(03).
                 15 DCOMW31E-E-CSPROD-DESC          PIC 9(03).
                 15 DCOMW31E-E-CONTRATO-LIM         PIC 9(09).
                 15 DCOMW31E-E-QTITLO               PIC 9(05).
                 15 DCOMW31E-E-CMEIO-ENTRD          PIC 9(03).
                 15 DCOMW31E-E-CTPO-DESC            PIC 9(03).
