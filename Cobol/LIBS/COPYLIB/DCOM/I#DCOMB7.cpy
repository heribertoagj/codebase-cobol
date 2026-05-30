      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0B7 - DB2PRD.THIST_OPER_ISENC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  B7-ARGUMENTOS-ENTRADA.

       05  B7-COLUNAS-DA-TABELA.
           10  B7-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  B7-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  B7-CTPO-ISENC-DESC              PIC S9(3)V COMP-3.
           10  B7-HULT-ATULZ                   PIC X(26).
           10  B7-CABRGC-ISENC-DESC            PIC S9(3)V COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  B7-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  B7-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  B7-RETORNO.
       05  B7-COD-RETORNO                         PIC 9(04).
       05  B7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  B7-PROXIMO-RESTART                     PIC 9(05).
       05  B7-QTDE-RETORNADA                      PIC 9(03).
       05  B7-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  B7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TB7.
           10  B7RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  B7RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  B7RT-CTPO-ISENC-DESC            PIC S9(3)V COMP-3.
           10  B7RT-HULT-ATULZ                 PIC X(26).
           10  B7RT-CABRGC-ISENC-DESC          PIC S9(3)V COMP-3.
