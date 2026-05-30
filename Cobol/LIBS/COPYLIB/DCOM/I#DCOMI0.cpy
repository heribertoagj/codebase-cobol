      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0I0 - DB2PRD.TTRASI_OPER_ISENC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  I0-ARGUMENTOS-ENTRADA.

       05  I0-COLUNAS-DA-TABELA.
           10  I0-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  I0-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  I0-CTPO-ISENC-DESC              PIC S9(3)V COMP-3.
           10  I0-HULT-ATULZ                   PIC X(26).
           10  I0-CABRGC-ISENC-DESC            PIC S9(3)V COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  I0-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  I0-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  I0-RETORNO.
       05  I0-COD-RETORNO                         PIC 9(04).
       05  I0-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  I0-PROXIMO-RESTART                     PIC 9(05).
       05  I0-QTDE-RETORNADA                      PIC 9(03).
       05  I0-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  I0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TI0.
           10  I0RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  I0RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  I0RT-CTPO-ISENC-DESC            PIC S9(3)V COMP-3.
           10  I0RT-HULT-ATULZ                 PIC X(26).
           10  I0RT-CABRGC-ISENC-DESC          PIC S9(3)V COMP-3.
