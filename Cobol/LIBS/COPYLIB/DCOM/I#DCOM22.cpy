      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB022 - DB2PRD.TDOCTO_FORML_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  22-ARGUMENTOS-ENTRADA.

       05  22-COLUNAS-DA-TABELA.
           10  22-CDOCTO-FORML-DESC            PIC X(10).
           10  22-CINDCD-MODLD-DESC            PIC X(1).
           10  22-CTPO-DOCTO-FORML             PIC S9(3)V COMP-3.
           10  22-IDOCTO-FORML-DESC            PIC X(40).
           10  22-IRSUMO-DOCTO-FORML           PIC X(15).
           10  22-CMOD-DOCTO-FORML             PIC S9(5)V COMP-3.
           10  22-QVIA-EMIS-DOCTO              PIC S9(1)V COMP-3.
           10  22-CINDCD-VIA-DIFER             PIC X(1).
           10  22-RDOCTO-FORML-DESC.
              49  22-RDOCTO-FORML-DESC-LEN     PIC S9(4) COMP.
              49  22-RDOCTO-FORML-DESC-TEXT    PIC X(240).
           10  22-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  22-HSIT-DESC-COML               PIC X(26).
           10  22-CTRANS-PROG-ATULZ            PIC X(8).
           10  22-HULT-ATULZ                   PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  22-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  22-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  22-RETORNO.
       05  22-COD-RETORNO                         PIC 9(04).
       05  22-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  22-PROXIMO-RESTART                     PIC 9(05).
       05  22-QTDE-RETORNADA                      PIC 9(03).
       05  22-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  22-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T22.
           10  22RT-CDOCTO-FORML-DESC          PIC X(10).
           10  22RT-CINDCD-MODLD-DESC          PIC X(1).
           10  22RT-CTPO-DOCTO-FORML           PIC S9(3)V COMP-3.
           10  22RT-IDOCTO-FORML-DESC          PIC X(40).
           10  22RT-IRSUMO-DOCTO-FORML         PIC X(15).
           10  22RT-CMOD-DOCTO-FORML           PIC S9(5)V COMP-3.
           10  22RT-QVIA-EMIS-DOCTO            PIC S9(1)V COMP-3.
           10  22RT-CINDCD-VIA-DIFER           PIC X(1).
           10  22RT-RDOCTO-FORML-DESC.
              49  22RT-RDOCTO-FORML-DESC-LEN   PIC S9(4) COMP.
              49  22RT-RDOCTO-FORML-DESC-TEXT  PIC X(240).
           10  22RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  22RT-HSIT-DESC-COML             PIC X(26).
           10  22RT-CTRANS-PROG-ATULZ          PIC X(8).
           10  22RT-HULT-ATULZ                 PIC X(26).
