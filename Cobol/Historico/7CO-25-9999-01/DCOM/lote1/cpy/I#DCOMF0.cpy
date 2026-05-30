      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0F0 - DB2PRD.TSPROD_GARNT_CREDT     *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  F0-ARGUMENTOS-ENTRADA.

       05  F0-COLUNAS-DA-TABELA.
           10  F0-CPRODT                       PIC S9(3)V COMP-3.
           10  F0-CSPROD-DESC-COML             PIC S9(3)V COMP-3.
           10  F0-CMEIO-ENTRD-DESC             PIC S9(3)V COMP-3.
           10  F0-CTPO-GARNT                   PIC S9(3)V COMP-3.
           10  F0-HINIC-VGCIA-DESC             PIC X(26).
           10  F0-HFIM-VGCIA-DESC              PIC X(26).
           10  F0-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  F0-HULT-ATULZ                   PIC X(26).
           10  F0-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  F0-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  F0-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  F0-RETORNO.
       05  F0-COD-RETORNO                         PIC 9(04).
       05  F0-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  F0-PROXIMO-RESTART                     PIC 9(05).
       05  F0-QTDE-RETORNADA                      PIC 9(03).
       05  F0-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  F0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TF0.
           10  F0RT-CPRODT                     PIC S9(3)V COMP-3.
           10  F0RT-CSPROD-DESC-COML           PIC S9(3)V COMP-3.
           10  F0RT-CMEIO-ENTRD-DESC           PIC S9(3)V COMP-3.
           10  F0RT-CTPO-GARNT                 PIC S9(3)V COMP-3.
           10  F0RT-HINIC-VGCIA-DESC           PIC X(26).
           10  F0RT-HFIM-VGCIA-DESC            PIC X(26).
           10  F0RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  F0RT-HULT-ATULZ                 PIC X(26).
           10  F0RT-CTERM                      PIC X(8).
