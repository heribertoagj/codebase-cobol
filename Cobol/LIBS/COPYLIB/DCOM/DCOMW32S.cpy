      ******************************************************************
      * NOME BOOK : DCOMW32S                                           *
      * DESCRICAO : BOOK DE SAIDA FEMP1A6C                             *
      * FUNCAO    : RECUPERAR DOCUMENTO PDF                            *
      * TIPO      : COORDENADOR                                        *
      * DATA      : 04/04/2023                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * EMPRESA   : FOURSYS                                            *
      * SISTEMA   : DCOM                                               *
      * TAMANHO   : 20028 BYTES                                        *
      ******************************************************************
      * DCOMW32S-S-QTDE-TOT-PAG       = QTD. TOTAL DE PAG. DO DOC.
      * DCOMW32S-S-NPROT-IMPRE-DOCTO  = NUM. DO PROTOCOLO DE IMPRESSAO *
      *                                 CONFORME O DICM                *
      * DCOMW32S-S-WTPLAT-IMPRE-DOCTO = PAG. DO DOC. PDF
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      * 00/00/0000 XXXXXXXXXXX       XXXXXXXXXXXXXXXXXXXXXXXXXXXX      *
      ******************************************************************
         05 DCOMW32S-HEADER.
            10 DCOMW32S-COD-LAYOUT          PIC X(008) VALUE 'DCOMW32S'.
            10 DCOMW32S-TAM-LAYOUT          PIC 9(005) VALUE 20028.
         05 DCOMW32S-REGISTRO.
            10 DCOMW32S-BLOCO-SAIDA.
               15 DCOMW32S-S-QTDE-TOT-PAG          PIC 9(05).
               15 DCOMW32S-S-NPROT-IMPRE-DOCTO     PIC 9(10).
               15 DCOMW32S-S-WTPLAT-IMPRE-DOCTO    PIC X(20000).
