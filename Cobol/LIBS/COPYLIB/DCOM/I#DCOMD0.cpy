      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0D0 - DB2PRD.TPRODT_MEIO_PGTO       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  D0-ARGUMENTOS-ENTRADA.

       05  D0-COLUNAS-DA-TABELA.
           10  D0-CPRODT                       PIC S9(3)V COMP-3.
           10  D0-CTPO-DESC-COML               PIC S9(3)V COMP-3.
           10  D0-CINDCD-VCTO-PCELA            PIC X(1).
           10  D0-CMEIO-PGTO-DESC              PIC S9(3)V COMP-3.
           10  D0-CTRANS-PROG-ATULZ            PIC X(8).
           10  D0-HULT-ATULZ                   PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  D0-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  D0-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  D0-RETORNO.
       05  D0-COD-RETORNO                         PIC 9(04).
       05  D0-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  D0-PROXIMO-RESTART                     PIC 9(05).
       05  D0-QTDE-RETORNADA                      PIC 9(03).
       05  D0-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  D0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TD0.
           10  D0RT-CPRODT                     PIC S9(3)V COMP-3.
           10  D0RT-CTPO-DESC-COML             PIC S9(3)V COMP-3.
           10  D0RT-CINDCD-VCTO-PCELA          PIC X(1).
           10  D0RT-CMEIO-PGTO-DESC            PIC S9(3)V COMP-3.
           10  D0RT-CTRANS-PROG-ATULZ          PIC X(8).
           10  D0RT-HULT-ATULZ                 PIC X(26).
