      ******************************************************************
      * NOME BOOK : DCOMWA6S                                           *
      * DESCRICAO : BOOK DE SAIDA FEMP1A6C                             *
      * FUNCAO    : RECUPERAR DOCUMENTO PDF                            *
      * TIPO      : COORDENADOR                                        *
      * DATA      : 03/12/2022                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * EMPRESA   : FOURSYS                                            *
      * SISTEMA   : DCOM                                               *
      * TAMANHO   : 20028 BYTES                                        *
      ******************************************************************
      * DCOMWA6S-S-QTDE-TOT-PAG       = QTD. TOTAL DE PAG. DO DOC.
      * DCOMWA6S-S-NPROT-IMPRE-DOCTO  = NUM. DO PROTOCOLO DE IMPRESSAO *
      *                                 CONFORME O DICM                *
      * DCOMWA6S-S-WTPLAT-IMPRE-DOCTO = PAG. DO DOC. PDF
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      * 00/00/0000 XXXXXXXXXXX       XXXXXXXXXXXXXXXXXXXXXXXXXXXX      *
      ******************************************************************
         05 DCOMWA6S-HEADER.
            10 DCOMWA6S-COD-LAYOUT          PIC X(008) VALUE 'DCOMWA6S'.
            10 DCOMWA6S-TAM-LAYOUT          PIC 9(005) VALUE 20028.
         05 DCOMWA6S-REGISTRO.
            10 DCOMWA6S-BLOCO-SAIDA.
               15 DCOMWA6S-S-QTDE-TOT-PAG          PIC 9(05).
               15 DCOMWA6S-S-NPROT-IMPRE-DOCTO     PIC 9(10).
               15 DCOMWA6S-S-WTPLAT-IMPRE-DOCTO    PIC X(20000).
