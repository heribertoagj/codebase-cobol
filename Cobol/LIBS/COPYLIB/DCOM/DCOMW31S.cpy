      ******************************************************************
      * NOME BOOK : DCOMW31S - BOOK DE SAIDA                           *
      * DESCRICAO : CONSULTA LISTA DE FORMULARIOS  IMPRESSAO SAIDA     *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00257 BYTES                                        *
      ******************************************************************
       05 DCOMW31S-HEADER.
          10 DCOMW31S-COD-LAYOUT             PIC X(08) VALUE 'DCOMW31S'.
          10 DCOMW31S-TAM-LAYOUT             PIC 9(05) VALUE 00257.
       05 DCOMW31S-REGISTRO.
          10 DCOMW31S-BLOCO-SAIDA.
             15 DCOMW31S-INDDOC              PIC X(01).
             15 DCOMW31S-DESCRICAO           PIC X(15).
             15 DCOMW31S-FORMULARIO          PIC X(10).
             15 DCOMW31S-QTDE-VIAS           PIC 9(01).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
