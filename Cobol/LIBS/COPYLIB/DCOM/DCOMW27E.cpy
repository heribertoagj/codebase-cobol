      ******************************************************************
      * NOME BOOK : DCOMW27E - BOOK DE ENTRADA                         *
      * DESCRICAO : IMPRIME FORMULARIO SELECIONADO IMPRESSAO           *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00037 BYTES                                        *
      ******************************************************************
           05 DCOMW27E-HEADER.
              10 DCOMW27E-COD-LAYOUT      PIC X(08) VALUE 'DCOMW27E'.
              10 DCOMW27E-TAM-LAYOUT      PIC 9(05) VALUE 00037.
           05 DCOMW27E-REGISTRO.
              10 DCOMW27E-AREA-ENTRADA.
                 15 DCOMW27E-E-COD-DOCTO                 PIC X(10).
                 15 DCOMW27E-E-NUM-OPER                  PIC 9(13).
                 15 DCOMW27E-TIPO-VIA                    PIC 9(1).
