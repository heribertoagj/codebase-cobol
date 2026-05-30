      ******************************************************************
      * NOME BOOK : DCOMW26E - BOOK DE ENTRADA                         *
      * DESCRICAO : CONSULTA LISTA DE FORMULARIOS  IMPRESSAO           *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00052 BYTES                                        *
      ******************************************************************
           05 DCOMW26E-HEADER.
              10 DCOMW26E-COD-LAYOUT      PIC X(08) VALUE 'DCOMW26E'.
              10 DCOMW26E-TAM-LAYOUT      PIC 9(05) VALUE 00052.
           05 DCOMW26E-REGISTRO.
              10 DCOMW26E-AREA-ENTRADA.
                 15 DCOMW26E-E-DANO-OPER            PIC 9(04).
                 15 DCOMW26E-E-NSEQ-OPER            PIC 9(09).
                 15 DCOMW26E-E-CPRODT               PIC 9(03).
                 15 DCOMW26E-E-CSPROD-DESC          PIC 9(03).
                 15 DCOMW26E-E-CONTRATO-LIM         PIC 9(09).
                 15 DCOMW26E-E-QTITLO               PIC 9(05).
                 15 DCOMW26E-E-CMEIO-ENTRD          PIC 9(03).
                 15 DCOMW26E-E-CTPO-DESC            PIC 9(03).
